## LEARN MORE TAB ##

learnMoreUI <- function() {
  tagList(
    includeMarkdown("md/learn_more.md"),
    accordion(
      accordion_panel(
        title = "Changelog",
        includeMarkdown("CHANGELOG.md")
      ),
      open = FALSE
    )
  )
}
