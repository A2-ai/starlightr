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
add_sidebar_item <- function(kind, slug, section, label = NULL, config_path = "_starlightr.toml") {
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
    config$sidebar[[kind]] <- c(config$sidebar[[kind]], list(new_section))
    write_config_toml(config, config_path)
    return(list(added = TRUE, new_section = TRUE))
  }

  # Check for duplicates by slug
  existing_contents <- config$sidebar[[kind]][[section_idx]]$contents
  existing_slugs <- vapply(existing_contents, function(c) {
    parse_content_item(c)$slug
  }, character(1))

  if (slug %in% existing_slugs) {
    return(list(added = FALSE, new_section = FALSE))
  }

  # Add to existing section
  config$sidebar[[kind]][[section_idx]]$contents <- c(existing_contents, list(content_item))
  write_config_toml(config, config_path)
  return(list(added = TRUE, new_section = FALSE))
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
#' }
add_reference <- function(fn_name, section, label = NULL, config_path = "_starlightr.toml") {
  result <- add_sidebar_item("reference", fn_name, section, label, config_path)

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
#' }
add_article <- function(vignette_name, section, label = NULL, config_path = "_starlightr.toml") {
  result <- add_sidebar_item("articles", vignette_name, section, label, config_path)

  if (!result$added) {
    cli::cli_alert_info("{.val {vignette_name}} already exists in section {.val {section}}")
  } else if (result$new_section) {
    cli::cli_alert_success("Created new section {.val {section}} with {.val {vignette_name}}")
  } else {
    cli::cli_alert_success("Added {.val {vignette_name}} to section {.val {section}}")
  }

  invisible(TRUE)
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
