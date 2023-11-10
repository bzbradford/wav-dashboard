## LEARN MORE TAB ##

learnMoreUI <- function() {
  tagList(
    includeMarkdown("md/learn_more.md"),
    br(),
    bsCollapse(
      bsCollapsePanel("Changelog", includeMarkdown("md/changelog.md"))
    )
  )
}
