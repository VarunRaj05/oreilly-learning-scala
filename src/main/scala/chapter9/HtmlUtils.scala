package chapter9


object HtmlUtils {
  def removeMarkup(input: String) = {
    input
      .replaceAll("""</?\w[^>]*>""","")
      .replaceAll("<.*>","")
  }
}