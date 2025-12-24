/-
  Tests for Scribe HTML builder
-/
import Scribe
import Crucible

namespace Tests.Builder

open Crucible
open Scribe

testSuite "Scribe HTML Builder"

-- Html Rendering Tests

test "text escapes HTML characters" := do
  let html := Html.text "<script>alert('xss')</script>"
  html.render ≡ "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"

test "raw does not escape" := do
  let html := Html.raw "<b>bold</b>"
  html.render ≡ "<b>bold</b>"

test "element renders opening and closing tags" := do
  let html := Html.element "div" [] []
  html.render ≡ "<div></div>"

test "element with text child" := do
  let html := Html.element "p" [] [.text "Hello"]
  html.render ≡ "<p>Hello</p>"

test "element with attributes" := do
  let html := Html.element "div" [⟨"class", "container"⟩, ⟨"id", "main"⟩] []
  html.render ≡ "<div class=\"container\" id=\"main\"></div>"

test "void elements have no closing tag" := do
  let html := Html.element "br" [] []
  html.render ≡ "<br>"

test "img is a void element" := do
  let html := Html.element "img" [⟨"src", "test.png"⟩] []
  html.render ≡ "<img src=\"test.png\">"

test "nested elements" := do
  let html := Html.element "div" [] [
    .element "span" [] [.text "Hello"],
    .element "span" [] [.text "World"]
  ]
  html.render ≡ "<div><span>Hello</span><span>World</span></div>"

test "attribute values are escaped" := do
  let html := Html.element "div" [⟨"data-value", "a\"b"⟩] []
  html.render ≡ "<div data-value=\"a&quot;b\"></div>"

test "fragment renders children without wrapper" := do
  let html := Html.fragment [.text "A", .text "B"]
  html.render ≡ "AB"

-- HtmlM Builder Tests

test "build creates fragment from children" := do
  let html := HtmlM.build (r := .volatile) (l := .nested) do
    HtmlM.text "Hello"
    HtmlM.text " World"
  html.render ≡ "Hello World"

test "element builder creates proper structure" := do
  let html := HtmlM.build (r := .volatile) (l := .nested) do
    div [] do
      text "Content"
  html.render ≡ "<div>Content</div>"

test "nested builder elements" := do
  let html := HtmlM.build (r := .volatile) (l := .nested) do
    div [] do
      p [] do
        text "Paragraph"
  html.render ≡ "<div><p>Paragraph</p></div>"

test "multiple children in builder" := do
  let html := HtmlM.build (r := .volatile) (l := .nested) do
    ul [] do
      li [] (text "One")
      li [] (text "Two")
      li [] (text "Three")
  html.render ≡ "<ul><li>One</li><li>Two</li><li>Three</li></ul>"

test "builder with attributes" := do
  let html := HtmlM.build (r := .volatile) (l := .nested) do
    div [class_ "container", id_ "main"] do
      text "Hello"
  html.render ≡ "<div class=\"container\" id=\"main\">Hello</div>"

test "control flow in builder - if" := do
  let showExtra := true
  let html := HtmlM.build (r := .volatile) (l := .nested) do
    div [] do
      text "Always"
      if showExtra then
        text " Extra"
  html.render ≡ "<div>Always Extra</div>"

test "control flow in builder - for" := do
  let items := ["A", "B", "C"]
  let html := HtmlM.build (r := .volatile) (l := .nested) do
    ul [] do
      for item in items do
        li [] (text item)
  html.render ≡ "<ul><li>A</li><li>B</li><li>C</li></ul>"

test "HtmlM.render produces string directly" := do
  let result := HtmlM.render (l := .nested) do
    p [] (text "Test")
  result ≡ "<p>Test</p>"

-- Attribute Helper Tests

test "class_ creates class attribute" := do
  let attr := class_ "container"
  (attr.name, attr.value) ≡ ("class", "container")

test "id_ creates id attribute" := do
  let attr := id_ "main"
  (attr.name, attr.value) ≡ ("id", "main")

test "href_ creates href attribute" := do
  let attr := href_ "https://example.com"
  (attr.name, attr.value) ≡ ("href", "https://example.com")

test "data_ creates data attribute" := do
  let attr := data_ "value" "42"
  (attr.name, attr.value) ≡ ("data-value", "42")

test "boolean attributes have empty value" := do
  let attr := required_
  (attr.name, attr.value) ≡ ("required", "")

-- HTML Element Tests

test "html element" := do
  let result := HtmlM.render (l := .nested) do
    html [] do
      text "Content"
  result ≡ "<html>Content</html>"

test "complete document structure" := do
  let result := HtmlM.render (l := .nested) do
    html [] do
      head [] do
        title "Test"
      body [] do
        text "Body"
  result ≡ "<html><head><title>Test</title></head><body>Body</body></html>"

test "link element with text" := do
  let result := HtmlM.render (l := .nested) do
    a [href_ "/page"] do
      text "Click me"
  result ≡ "<a href=\"/page\">Click me</a>"

test "form elements" := do
  -- form requires stable region
  let result := HtmlM.renderStable (l := .nested) do
    form [action_ "/submit", method_ "POST"] do
      input [type_ "text", name_ "username"]
      button [type_ "submit"] (text "Submit")
  result ≡ "<form action=\"/submit\" method=\"POST\"><input type=\"text\" name=\"username\"><button type=\"submit\">Submit</button></form>"

test "table structure" := do
  let result := HtmlM.render (l := .nested) do
    table [] do
      tr [] do
        th [] (text "Header")
      tr [] do
        td [] (text "Cell")
  result ≡ "<table><tr><th>Header</th></tr><tr><td>Cell</td></tr></table>"

test "image element" := do
  let result := HtmlM.render (l := .nested) do
    img [src_ "photo.jpg", alt_ "A photo"]
  result ≡ "<img src=\"photo.jpg\" alt=\"A photo\">"

test "br element" := do
  let result := HtmlM.render (l := .nested) do
    p [] do
      text "Line 1"
      br
      text "Line 2"
  result ≡ "<p>Line 1<br>Line 2</p>"

#generate_tests

end Tests.Builder
