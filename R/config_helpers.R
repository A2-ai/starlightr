# Helper functions for programmatic config modification

# ---- Internal I/O helpers ----

#' Read and parse TOML config file
#'
#' @param config_path Path to config file
#' @return Parsed config as list
#' @keywords internal
read_config_toml <- function(config_path = "_starlightr.toml") {
  if (!file.exists(config_path)) {
    cli::cli_abort("Configuration file not found at {.path {config_path}}")
  }
  toml_obj <- tomledit::read_toml(config_path)
  tomledit::from_toml(toml_obj)
}

#' Write config list back to TOML file
#'
#' @param config Config list to write
#' @param config_path Path to config file
#' @keywords internal
write_config_toml <- function(config, config_path = "_starlightr.toml") {
  toml_out <- tomledit::as_toml(config)
  tomledit::write_toml(toml_out, config_path)
}

#' Add item to a sidebar section (internal helper)
#'
#' @param kind "reference" or "articles"
#' @param slug Item slug/name
#' @param section Section label
#' @param label Optional display label
#' @param config_path Path to config file
#' @return List with modified config and whether item was added
#' @keywords internal
add_sidebar_item <- function(kind, slug, section, label = NULL,
                             config_path = "_starlightr.toml", collapsed = NULL) {
  config <- read_config_toml(config_path)

  # Initialize sidebar section if it doesn't exist
  if (is.null(config$sidebar)) {
    config$sidebar <- list()
  }
  if (is.null(config$sidebar[[kind]])) {
    config$sidebar[[kind]] <- list()
  }

  # Find the section by label
  section_idx <- NULL
  for (i in seq_along(config$sidebar[[kind]])) {
    if (!is.list(config$sidebar[[kind]][[i]])) next
    if (!is.null(config$sidebar[[kind]][[i]]$label) &&
        config$sidebar[[kind]][[i]]$label == section) {
      section_idx <- i
      break
    }
  }

  # Create content item (string or list with slug/label)
  content_item <- if (!is.null(label)) {
    list(slug = slug, label = label)
  } else {
    slug
  }

  if (is.null(section_idx)) {
    # Create new section
    new_section <- list(label = section, contents = list(content_item))
    if (isTRUE(collapsed)) new_section$collapsed <- TRUE
    config$sidebar[[kind]] <- c(config$sidebar[[kind]], list(new_section))
    write_config_toml(config, config_path)
    return(list(added = TRUE, new_section = TRUE))
  }

  # Apply collapsed if requested
  if (!is.null(collapsed)) {
    if (isTRUE(collapsed)) {
      config$sidebar[[kind]][[section_idx]]$collapsed <- TRUE
    } else {
      config$sidebar[[kind]][[section_idx]]$collapsed <- NULL
    }
  }

  # Check for duplicates by slug
  existing_contents <- config$sidebar[[kind]][[section_idx]]$contents
  existing_slugs <- vapply(existing_contents, function(c) {
    parse_content_item(c)$slug
  }, character(1))

  if (slug %in% existing_slugs) {
    # Still write if collapsed changed
    if (!is.null(collapsed)) write_config_toml(config, config_path)
    return(list(added = FALSE, new_section = FALSE))
  }

  # Add to existing section
  config$sidebar[[kind]][[section_idx]]$contents <- c(existing_contents, list(content_item))
  write_config_toml(config, config_path)
  return(list(added = TRUE, new_section = FALSE))
}

#' Get sidebar sections as a data frame (internal helper)
#'
#' @param kind "reference" or "articles"
#' @param config_path Path to config file
#' @return Data frame with columns: label, collapsed, n_items
#' @keywords internal
get_sidebar_sections <- function(kind, config_path = "_starlightr.toml") {
  config <- read_config_toml(config_path)
  sections <- config$sidebar[[kind]]

  if (is.null(sections) || length(sections) == 0) {
    return(data.frame(
      label = character(0),
      collapsed = logical(0),
      n_items = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  labels <- vapply(sections, function(s) {
    if (is.list(s)) s$label %||% NA_character_ else as.character(s)
  }, character(1))
  collapsed <- vapply(sections, function(s) {
    if (is.list(s)) isTRUE(s$collapsed) else NA
  }, logical(1))
  n_items <- vapply(sections, function(s) {
    if (is.list(s)) length(s$contents) else 1L
  }, integer(1))

  data.frame(
    label = labels,
    collapsed = collapsed,
    n_items = n_items,
    stringsAsFactors = FALSE
  )
}

#' Set collapsed state for sidebar sections (internal helper)
#'
#' @param kind "reference" or "articles"
#' @param section Character vector of section labels
#' @param collapsed Logical, whether sections should be collapsed
#' @param config_path Path to config file
#' @return Character vector of labels actually modified (invisibly)
#' @keywords internal
set_sidebar_section_collapsed <- function(kind, section, collapsed = TRUE,
                                          config_path = "_starlightr.toml") {
  config <- read_config_toml(config_path)
  sections <- config$sidebar[[kind]]

  if (is.null(sections) || length(sections) == 0) {
    cli::cli_warn("No {.val {kind}} sections found in {.path {config_path}}")
    return(invisible(character(0)))
  }

  modified <- character(0)

  for (lbl in section) {
    found <- FALSE
    for (i in seq_along(sections)) {
      if (!is.list(sections[[i]])) next
      if (!is.null(sections[[i]]$label) && sections[[i]]$label == lbl) {
        found <- TRUE
        current <- isTRUE(sections[[i]]$collapsed)
        if (current == collapsed) next
        if (collapsed) {
          config$sidebar[[kind]][[i]]$collapsed <- TRUE
        } else {
          config$sidebar[[kind]][[i]]$collapsed <- NULL
        }
        modified <- c(modified, lbl)
        break
      }
    }
    if (!found) {
      cli::cli_warn("Section {.val {lbl}} not found in {.val {kind}} sidebar")
    }
  }

  if (length(modified) > 0) {
    write_config_toml(config, config_path)
  }

  invisible(modified)
}

# ---- Exported helpers ----

#' Add a card to the home page
#'
#' Adds a card to the home page cards section in the starlightr configuration file.
#'
#' @param title Card title
#' @param description Card description text
#' @param link Link URL for the card
#' @param icon Icon name (e.g., "document", "open-book", "seti:csv")
#' @param overwrite Logical, whether to overwrite existing card with same title
#' @param config_path Path to config file (default: "_starlightr.toml")
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples \dontrun{
#' # Add a card to the home page
#' add_card(
#'   title = "Getting Started",
#'   description = "Learn how to use the package",
#'   link = "./articles/introduction/",
#'   icon = "rocket"
#' )
#'
#' # Update an existing card
#' add_card(
#'   title = "Getting Started",
#'   description = "Updated description",
#'   link = "./articles/intro/",
#'   overwrite = TRUE
#' )
#' }
add_card <- function(title, description, link, icon = "document", overwrite = FALSE, config_path = "_starlightr.toml") {
  config <- read_config_toml(config_path)

  # Initialize home.cards if it doesn't exist
  if (is.null(config$home)) {
    config$home <- list()
  }
  if (is.null(config$home$cards)) {
    config$home$cards <- list()
  }

  # Check for duplicate by title
  existing_idx <- NULL
  for (i in seq_along(config$home$cards)) {
    if (!is.null(config$home$cards[[i]]$title) && config$home$cards[[i]]$title == title) {
      existing_idx <- i
      break
    }
  }

  if (!is.null(existing_idx) && !overwrite) {
    cli::cli_alert_info("Card with title {.val {title}} already exists (use overwrite = TRUE to update)")
    return(invisible(TRUE))
  }

  # Normalize local links to lowercase
  link <- normalize_local_link(link)

  # Create new card
  new_card <- list(
    icon = icon,
    title = title,
    description = description,
    link = link
  )

  if (!is.null(existing_idx)) {
    # Overwrite existing card
    config$home$cards[[existing_idx]] <- new_card
    cli::cli_alert_success("Updated card {.val {title}}")
  } else {
    # Add new card
    config$home$cards <- c(config$home$cards, list(new_card))
    cli::cli_alert_success("Added card {.val {title}}")
  }

  write_config_toml(config, config_path)
  invisible(TRUE)
}

#' Add a hero action to the home page
#'
#' Adds an action button to the home page hero section in the starlightr configuration file.
#'
#' @param text Button text
#' @param link Link URL for the action
#' @param icon Icon name (e.g., "right-arrow", "external")
#' @param variant Button variant ("primary" or "minimal")
#' @param overwrite Logical, whether to overwrite existing action with same text
#' @param config_path Path to config file (default: "_starlightr.toml")
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples \dontrun{
#' # Add primary action
#' add_action(
#'   text = "Get Started",
#'   link = "./articles/readme/",
#'   icon = "right-arrow",
#'   variant = "primary"
#' )
#'
#' # Add external link action
#' add_action(
#'   text = "View on GitHub",
#'   link = "https://github.com/user/repo",
#'   icon = "external",
#'   variant = "minimal"
#' )
#' }
add_action <- function(text, link, icon = "right-arrow", variant = "primary",
                       overwrite = FALSE, config_path = "_starlightr.toml") {
  config <- read_config_toml(config_path)

  # Initialize home.hero.actions if needed
  if (is.null(config$home)) {
    config$home <- list()
  }
  if (is.null(config$home$hero)) {
    config$home$hero <- list()
  }
  if (is.null(config$home$hero$actions)) {
    config$home$hero$actions <- list()
  }

  # Check for duplicate by text
  existing_idx <- NULL
  for (i in seq_along(config$home$hero$actions)) {
    if (!is.null(config$home$hero$actions[[i]]$text) &&
        config$home$hero$actions[[i]]$text == text) {
      existing_idx <- i
      break
    }
  }

  if (!is.null(existing_idx) && !overwrite) {
    cli::cli_alert_info("Action with text {.val {text}} already exists (use overwrite = TRUE to update)")
    return(invisible(TRUE))
  }

  # Normalize local links to lowercase
  link <- normalize_local_link(link)

  # Create new action
  new_action <- list(
    text = text,
    link = link,
    icon = icon,
    variant = variant
  )

  if (!is.null(existing_idx)) {
    config$home$hero$actions[[existing_idx]] <- new_action
    cli::cli_alert_success("Updated action {.val {text}}")
  } else {
    config$home$hero$actions <- c(config$home$hero$actions, list(new_action))
    cli::cli_alert_success("Added action {.val {text}}")
  }

  write_config_toml(config, config_path)
  invisible(TRUE)
}

#' Add a function to a reference section
#'
#' Adds a function name to the specified reference section in the starlightr
#' configuration file. Creates the section if it doesn't exist.
#'
#' @param fn_name Function name to add
#' @param section Section label to add to (creates if doesn't exist)
#' @param label Optional display label (if different from fn_name)
#' @param config_path Path to config file (default: "_starlightr.toml")
#' @param collapsed Optional logical to set collapsed state of the section
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples \dontrun{
#' # Add a function to an existing section
#' add_reference("my_function", "Site Building")
#'
#' # Add with a custom display label
#' add_reference("my_function", "Site Building", label = "My Function")
#'
#' # Add to a collapsed section
#' add_reference("my_function", "Internals", collapsed = TRUE)
#' }
add_reference <- function(fn_name, section, label = NULL,
                          config_path = "_starlightr.toml", collapsed = NULL) {
  if (!is.character(fn_name) || length(fn_name) != 1 || !nzchar(trimws(fn_name))) {
    cli::cli_abort("{.arg fn_name} must be a non-empty string")
  }
  if (!is.character(section) || length(section) != 1 || !nzchar(trimws(section))) {
    cli::cli_abort("{.arg section} must be a non-empty string")
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1 || !nzchar(trimws(label)))) {
    cli::cli_abort("{.arg label} must be NULL or a non-empty string")
  }

  result <- add_sidebar_item("reference", fn_name, section, label, config_path, collapsed)

  if (!result$added) {
    cli::cli_alert_info("{.fn {fn_name}} already exists in section {.val {section}}")
  } else if (result$new_section) {
    cli::cli_alert_success("Created new section {.val {section}} with {.fn {fn_name}}")
  } else {
    cli::cli_alert_success("Added {.fn {fn_name}} to section {.val {section}}")
  }

  invisible(TRUE)
}

#' Add a vignette to an articles section
#'
#' Adds a vignette name to the specified articles section in the starlightr
#' configuration file. Creates the section if it doesn't exist.
#'
#' @param vignette_name Vignette name (without .Rmd extension)
#' @param section Section label to add to (creates if doesn't exist)
#' @param label Optional display label (if different from vignette_name)
#' @param config_path Path to config file (default: "_starlightr.toml")
#' @param collapsed Optional logical to set collapsed state of the section
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples \dontrun{
#' # Add a vignette to an existing section
#' add_article("my-vignette", "Getting Started")
#'
#' # Add with a custom display label
#' add_article("README", "Getting Started", label = "About")
#'
#' # Add to a collapsed section
#' add_article("advanced-usage", "Advanced", collapsed = TRUE)
#' }
add_article <- function(vignette_name, section, label = NULL,
                        config_path = "_starlightr.toml", collapsed = NULL) {
  if (!is.character(vignette_name) || length(vignette_name) != 1 || !nzchar(trimws(vignette_name))) {
    cli::cli_abort("{.arg vignette_name} must be a non-empty string")
  }
  if (!is.character(section) || length(section) != 1 || !nzchar(trimws(section))) {
    cli::cli_abort("{.arg section} must be a non-empty string")
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1 || !nzchar(trimws(label)))) {
    cli::cli_abort("{.arg label} must be NULL or a non-empty string")
  }

  result <- add_sidebar_item("articles", vignette_name, section, label, config_path, collapsed)

  if (!result$added) {
    cli::cli_alert_info("{.val {vignette_name}} already exists in section {.val {section}}")
  } else if (result$new_section) {
    cli::cli_alert_success("Created new section {.val {section}} with {.val {vignette_name}}")
  } else {
    cli::cli_alert_success("Added {.val {vignette_name}} to section {.val {section}}")
  }

  invisible(TRUE)
}

#' Get reference sidebar sections
#'
#' Returns a data frame describing each section in the reference sidebar,
#' including its label, collapsed state, and number of items.
#'
#' @param config_path Path to config file (default: "_starlightr.toml")
#'
#' @return A data frame with columns: label, collapsed, n_items
#' @export
#'
#' @examples \dontrun{
#' get_reference_sections()
#' }
get_reference_sections <- function(config_path = "_starlightr.toml") {
  get_sidebar_sections("reference", config_path)
}

#' Get article sidebar sections
#'
#' Returns a data frame describing each section in the articles sidebar,
#' including its label, collapsed state, and number of items.
#'
#' @param config_path Path to config file (default: "_starlightr.toml")
#'
#' @return A data frame with columns: label, collapsed, n_items
#' @export
#'
#' @examples \dontrun{
#' get_article_sections()
#' }
get_article_sections <- function(config_path = "_starlightr.toml") {
  get_sidebar_sections("articles", config_path)
}

#' Set collapsed state for reference sidebar sections
#'
#' Sets the collapsed property for one or more reference sidebar sections
#' in the starlightr configuration file.
#'
#' @param section Character vector of section labels to modify
#' @param collapsed Logical, whether sections should be collapsed (default: TRUE)
#' @param config_path Path to config file (default: "_starlightr.toml")
#'
#' @return Invisibly returns character vector of labels actually modified
#' @export
#'
#' @examples \dontrun{
#' # Collapse a section
#' set_reference_section("Site Building", collapsed = TRUE)
#'
#' # Expand a section
#' set_reference_section("Site Building", collapsed = FALSE)
#'
#' # Collapse multiple sections
#' set_reference_section(c("Site Building", "Migration"), collapsed = TRUE)
#' }
set_reference_section <- function(section, collapsed = TRUE,
                                  config_path = "_starlightr.toml") {
  modified <- set_sidebar_section_collapsed("reference", section, collapsed, config_path)
  state <- if (collapsed) "collapsed" else "expanded"
  for (lbl in modified) {
    cli::cli_alert_success("Set {.val {lbl}} to {state}")
  }
  invisible(modified)
}

#' Set collapsed state for article sidebar sections
#'
#' Sets the collapsed property for one or more article sidebar sections
#' in the starlightr configuration file.
#'
#' @param section Character vector of section labels to modify
#' @param collapsed Logical, whether sections should be collapsed (default: TRUE)
#' @param config_path Path to config file (default: "_starlightr.toml")
#'
#' @return Invisibly returns character vector of labels actually modified
#' @export
#'
#' @examples \dontrun{
#' # Collapse a section
#' set_article_section("Getting Started", collapsed = TRUE)
#'
#' # Expand a section
#' set_article_section("Getting Started", collapsed = FALSE)
#' }
set_article_section <- function(section, collapsed = TRUE,
                                config_path = "_starlightr.toml") {
  modified <- set_sidebar_section_collapsed("articles", section, collapsed, config_path)
  state <- if (collapsed) "collapsed" else "expanded"
  for (lbl in modified) {
    cli::cli_alert_success("Set {.val {lbl}} to {state}")
  }
  invisible(modified)
}

#' Add an npm package dependency
#'
#' Adds an npm package to the dependencies section of the starlightr
#' configuration file. If the output \code{package.json} already exists,
#' it is patched immediately.
#'
#' @param name Package name (e.g., "starlight-links-validator")
#' @param version Version spec (e.g., "^0.12.3")
#' @param config_path Path to config file (default: "_starlightr.toml")
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples \dontrun{
#' add_package("starlight-links-validator", "^0.12.3")
#' }
add_package <- function(name, version, config_path = "_starlightr.toml") {
  if (!is.character(name) || length(name) != 1 || !nzchar(trimws(name))) {
    cli::cli_abort("{.arg name} must be a non-empty string")
  }
  if (!is.character(version) || length(version) != 1 || !nzchar(trimws(version))) {
    cli::cli_abort("{.arg version} must be a non-empty string")
  }

  config <- read_config_toml(config_path)

  # Initialize dependencies list if absent
  if (is.null(config$dependencies)) {
    config$dependencies <- list()
  }

  # Check for duplicate by name
  for (i in seq_along(config$dependencies)) {
    dep <- config$dependencies[[i]]
    if (!is.null(dep$name) && dep$name == name) {
      if (dep$version == version) {
        cli::cli_alert_info("{.pkg {name}} already at version {.val {version}}")
        return(invisible(TRUE))
      }
      # Different version — update in place
      config$dependencies[[i]]$version <- version
      cli::cli_alert_success("Updated {.pkg {name}} to {.val {version}}")
      write_config_toml(config, config_path)
      patch_package_json(config, config_path)
      return(invisible(TRUE))
    }
  }

  # Append new dependency
  config$dependencies <- c(config$dependencies, list(list(name = name, version = version)))
  cli::cli_alert_success("Added {.pkg {name}} {.val {version}}")

  write_config_toml(config, config_path)
  patch_package_json(config, config_path)
  invisible(TRUE)
}

#' Patch output package.json with current config dependencies
#'
#' If the output directory contains a package.json, merge deps into it.
#'
#' @param config Configuration list
#' @param config_path Path to config file (used to resolve output dir)
#' @keywords internal
patch_package_json <- function(config, config_path) {
  output_dir <- config$output$dir %||% "docs"
  if (is_absolute_path(output_dir)) {
    pkg_json <- file.path(output_dir, "package.json")
  } else {
    pkg_json <- file.path(dirname(config_path), output_dir, "package.json")
  }

  if (!file.exists(pkg_json)) return(invisible(NULL))

  merge_package_deps(pkg_json, config)
  cli::cli_alert_success("Patched {.file package.json}")
}

#' Add a version to the versions list
#'
#' Adds a version entry to the versions.list section in the starlightr
#' configuration file. Enables versioning if not already enabled.
#'
#' @param tag Git tag for the version (e.g., "v1.2.0", "dev")
#' @param label Display label (defaults to tag value)
#' @param default Logical, whether this is the default version
#' @param config_path Path to config file (default: "_starlightr.toml")
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples \dontrun{
#' # Add a new version
#' add_version("v1.2.0")
#'
#' # Add with custom label
#' add_version("v1.2.0", label = "v1.2.0 (latest)", default = TRUE)
#'
#' # Add dev version
#' add_version("dev", label = "Development")
#' }
add_version <- function(tag, label = NULL, default = FALSE, config_path = "_starlightr.toml") {
  config <- read_config_toml(config_path)

  # Initialize versions section if it doesn't exist
  if (is.null(config$versions)) {
    config$versions <- list(enabled = TRUE, list = list())
    cli::cli_alert_info("Enabled versioning support")
  }
  if (is.null(config$versions$list)) {
    config$versions$list <- list()
  }
  if (!isTRUE(config$versions$enabled)) {
    config$versions$enabled <- TRUE
    cli::cli_alert_info("Enabled versioning support")
  }

  # Check for duplicate tag
  for (v in config$versions$list) {
    if (!is.null(v$tag) && v$tag == tag) {
      cli::cli_alert_info("Version {.val {tag}} already exists")
      return(invisible(TRUE))
    }
  }

  # Set default label if not provided
  if (is.null(label)) {
    label <- if (tag == "dev") "Development" else tag
  }

  # If this is default, remove default from others
  if (default) {
    for (i in seq_along(config$versions$list)) {
      config$versions$list[[i]]$default <- NULL
    }
  }

  # Create new version entry
  new_version <- list(
    tag = tag,
    label = label
  )
  if (default) {
    new_version$default <- TRUE
  }

  config$versions$list <- c(config$versions$list, list(new_version))
  cli::cli_alert_success("Added version {.val {tag}}")

  write_config_toml(config, config_path)

  invisible(TRUE)
}
