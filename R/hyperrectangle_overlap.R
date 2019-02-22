##############
# functions adapting xrf rulesets to generic volumes & back
##############
features_to_space_identifier <- function(features) {
  # TODO, if we choose to use this pattern, need csv escaping - on a plane and can't google :)
  ordered_features <- sort(unique(features))
  return(paste(ordered_features, collapse=','))
}

resolve_splits_to_bounding <- function(split, less_than) {
  lower_indices <- which(less_than)
  upper_indices <- which(!less_than)
  lower_bound <- max(c(-Inf, split[upper_indices]))
  upper_bound <- min(c(Inf, split[lower_indices]))
  
  list(lower_bound = lower_bound, upper_bound = upper_bound)
}

build_volumes_from_xrf_rules <- function(rules) {
  # turn all rules into bounds. for singly split dimensions, this means adding the appropriate Inf bound
  # for dimensions split many times, shrink to smallest bound (since a rule is a conjunction)
  rules_as_bounds <- rules %>%
    group_by(rule_id, feature) %>%
    summarize(
      lower_bound = resolve_splits_to_bounding(split, less_than)$lower_bound,
      upper_bound = resolve_splits_to_bounding(split, less_than)$upper_bound
    )
    
  # create a logical grouping of volumes that occupy the same space (i.e. could feasbily be overlapped)
  space_assigned_volumes <- rules_as_bounds %>%
    group_by(rule_id) %>%
    mutate(
      space_id = features_to_space_identifier(feature)
    ) %>%
    ungroup()
  
  # rename columns for generic algo to solve
  space_assigned_volumes %>%
    mutate(
      dimension = feature,
      volume_id = rule_id,
      min = lower_bound,
      max = upper_bound
    ) %>%
    select(dimension, volume_id, min, max, space_id)
}

build_xrf_rules_from_volumes <- function(volumes) {
  rbind(
    volumes %>%
      filter(is.finite(min)) %>%
      mutate(
        rule_id = make.names(volume_id),
        feature = dimension,
        split = min,
        less_than = FALSE
      ),
    volumes %>%
      filter(is.finite(max)) %>%
      mutate(
        rule_id = make.names(volume_id),
        feature = dimension,
        split = max,
        less_than = TRUE
      ),
    stringsAsFactors = FALSE
  )
}

##################
# generic deoverlapping algo
##################

build_fully_partitioned_space <- function(volumes) {
  volumes %>% 
    mutate(bound = min) %>%
    select(dimension, bound) %>%
    rbind(
      volumes %>% 
        mutate(bound = max) %>%
        select(dimension, bound),
      stringsAsFactors = FALSE
    )
}

generate_volumes_from_partitioned_space <- function(partitioned_space, id_starter = 1) {
  if (nrow(partitioned_space) == 0) {
    return(data.frame())
  }
  
  # pick an arbtirary first dimension
  dimension_of_interest <- partitioned_space$dimension[1]
  dimension_bounds <- partitioned_space %>% 
    filter(dimension == dimension_of_interest) %>%
    # this is a small optimization - equal bounds are redundant
    distinct() %>%
    arrange(bound)
  
  # there should always be 2 or more, since each bound corresponds to hyperrectangle edge
  stopifnot(nrow(dimension_bounds) > 1)
  
  # subspace meaning everything outside the dimension of interest
  partitioned_subspace <- partitioned_space %>% filter(dimension != dimension_of_interest)
  # recursively build ranges from the subspace before tacking on ranges for the dimension of interest in this stack frame
  subspace_volumes <- generate_volumes_from_partitioned_space(partitioned_subspace, id_starter = id_starter)
  
  # "expanded" by the dimension of interest, that is 
  expanded_volumes <- data.frame()
  for (bound_ix in 1:(nrow(dimension_bounds) - 1)) {
    # note that we are iterating on the sorted bounds
    lower_bound <- dimension_bounds$bound[bound_ix]
    upper_bound <- dimension_bounds$bound[bound_ix + 1]
    
    if (nrow(subspace_volumes) == 0) {
      # case this is the first dimension - there's nothing to add onto
      volume_id <- paste0(id_starter, '_', dimension_of_interest, '_', bound_ix)
      new_dimension_bounds <- list(volume_id = volume_id, 
                                   min = lower_bound, 
                                   max = upper_bound,
                                   dimension = dimension_of_interest)
    }
    else {
      # case this is after the first dimension - create a new volume for each subspace volume with the new bounds added (cartesian product)
      new_dimension_bounds <- lapply(unique(subspace_volumes$volume_id), function(volume_id) {
        list(volume_id = paste0(volume_id, '_', dimension_of_interest, '_', bound_ix), # TODO this form of creating an ID could get costly in higher dimensions
             min = lower_bound, 
             max = upper_bound,
             dimension = dimension_of_interest)
      }) %>% bind_rows() %>%
        rbind(subspace_volumes %>%
                mutate(volume_id = paste0(volume_id, '_', dimension_of_interest, '_', bound_ix)),
              stringsAsFactors= FALSE)
    }
    
    expanded_volumes <- rbind(expanded_volumes, new_dimension_bounds,
                              stringsAsFactors = FALSE)
  }
  
  return(expanded_volumes)
}

prune_uncovering_volumes <- function(new_volumes, original_volumes) {
  # we left join because not all new volumes belong to all old volumes
  # the range join prescribes that the original volumes contains the new volume
  original_to_new_volumes <- fuzzy_left_join(original_volumes, new_volumes,
                                             by = c('min' = 'min',
                                                    'max' = 'max',
                                                    'dimension' = 'dimension'),
                                             match_fun = c(`<=`, `>=`, `==`)) %>%
    # renaming some things in a reasonable way
    mutate(dimension = dimension.x) %>%
    select(-dimension.x, -dimension.y)
  
  covering_volumes <- data.frame()
  for (new_volume_id_to_check in unique(new_volumes$volume_id)) {
    volume <- new_volumes %>%
      filter(volume_id == new_volume_id_to_check)
    
    in_covering_space <- FALSE
    for (original_volume_id_to_check in unique(original_volumes$volume_id)) {
      original_volume_to_check <- original_to_new_volumes %>%
        filter(volume_id.x == original_volume_id_to_check)
      # here we make sure all dimensions are contained
      volume_dimensions_contained <- original_to_new_volumes %>%
        filter(volume_id.x == original_volume_id_to_check & 
                 volume_id.y == new_volume_id_to_check) %>%
        pull(dimension) %>% 
        setequal(original_volume_to_check$dimension)
      
      if (volume_dimensions_contained) {
        in_covering_space <- TRUE
        break
      }
    }
    
    if (in_covering_space) {
      covering_volumes <- rbind(covering_volumes,
                                volume,
                                stringsAsFactors = FALSE)
    }
  }
  
  covering_volumes
}


fuse_abutted_hyperrectangles <- function(volumes, original_volumes) {
  dimensionality <- n_distinct(volumes$dimension)
  
  fused_volumes <- volumes
  fuses_possible <- TRUE
  fused_volume_unique_id <- 1
  
  while (fuses_possible) {
    fuses_possible <- FALSE
    
    candidate_fuses <- fused_volumes %>%
      inner_join(fused_volumes, by = c('dimension' = 'dimension',
                                       'max' = 'min'),
                 suffix = c('.left', '.right')) %>%
      filter(volume_id.left != volume_id.right) %>%  # this should only happen if a range is of size 0
      mutate(
        max = max.right # since the left max (where the abuttment happens on the right min) must be less than the right max
      ) %>%
      distinct(dimension, volume_id.left, volume_id.right, min, max) 
    
    # note this is a one to many maping, since the originals are overlapped
    current_volumes_to_original <- fused_volumes %>%
      fuzzy_inner_join(original_volumes, by = c('min' = 'min',
                                                'max' = 'max',
                                                'dimension' = 'dimension'),
                       match_fun = c(`>=`, `<=`, `==`)) %>%
      group_by(volume_id.x, volume_id.y) %>%
      filter(n_distinct(dimension.x) == dimensionality) %>%
      summarize(
        volume_id = volume_id.x[1],
        original_volume_id = volume_id.y[1]
      ) %>%
      ungroup() %>%
      select(volume_id, original_volume_id)
    
    for (candidate_fuse_ix in seq_len(nrow(candidate_fuses))) {
      candidate_fuse <- candidate_fuses[candidate_fuse_ix, ]
      # subvolume because we ignore the dimension of the fuse
      subvolume_left <- fused_volumes %>%
        filter(volume_id == candidate_fuse$volume_id.left & dimension != candidate_fuse$dimension)
      subvolume_right <- fused_volumes %>%
        filter(volume_id == candidate_fuse$volume_id.right & dimension != candidate_fuse$dimension)
      
      # this case implies the volume has already been joined
      # meaning the candidate fuse may not be valid any longer - catch it next iteration
      if (nrow(subvolume_left) == 0 || nrow(subvolume_right) == 0) {
        next()
      }
      
      stopifnot(nrow(subvolume_left) == nrow(subvolume_right) && 
                  n_distinct(subvolume_left$dimension) == n_distinct(subvolume_right$dimension))
      
      dimension_matches <- subvolume_left %>%
        inner_join(subvolume_right, by = c('dimension' = 'dimension',
                                           'min' = 'min',
                                           'max' = 'max'))
      
      original_volume_counts <- current_volumes_to_original %>%
        filter(volume_id %in% c(candidate_fuse$volume_id.left, candidate_fuse$volume_id.right)) %>%
        group_by(original_volume_id) %>%
        count() %>%
        pull(n)
      
      if (nrow(dimension_matches) == dimensionality - 1 &&
          all(original_volume_counts == 2)) { #
        fuses_possible <- TRUE
        
        # add in the new volume
        fused_volume <- rbind(
          dimension_matches %>% select(min, max, dimension),
          candidate_fuse %>% select(min, max, dimension),
          stringsAsFactors = FALSE)
        fused_volume$volume_id <- paste0(candidate_fuse$volume_id.left, '_', 
                                         candidate_fuse$volume_id.right, '_', 
                                         as.character(fused_volume_unique_id))
        fused_volume_unique_id <- fused_volume_unique_id + 1
        fused_volumes <- rbind(fused_volumes,
                               fused_volume,
                               stringsAsFactors = FALSE)
        
        # clean up the old volumes
        fused_volumes <- fused_volumes %>%
          filter(volume_id != candidate_fuse$volume_id.left & volume_id != candidate_fuse$volume_id.right)
      }
    }
  }
  
  return(fused_volumes)
}

deoverlap_hyperrectangles <- function(volumes) {
  partitioned_space <- build_fully_partitioned_space(volumes)
  new_volumes <- generate_volumes_from_partitioned_space(partitioned_space)
  solution <- prune_uncovering_volumes(new_volumes, volumes)
  fuse_abutted_hyperrectangles(solution, volumes)
}

xrf_deoverlap_rules <- function(rules) {
  volumes <- build_volumes_from_xrf_rules(rules)
  deoverlapped_volumes <- data.frame()
  for (deoverlap_space_id in unique(volumes$space_id)) {
    volumes_in_space <- volumes %>% 
      filter(deoverlap_space_id == space_id) %>%
      select(-space_id)
    deoverlapped_volumes <- rbind(deoverlapped_volumes, 
                                  deoverlap_hyperrectangles(volumes_in_space),
                                  stringsAsFactors = FALSE)
  }
  
  build_xrf_rules_from_volumes(deoverlapped_volumes)
}