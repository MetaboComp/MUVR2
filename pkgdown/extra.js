// Inline the SVG figures.
//
// The article figures are drawn with svglite, so the text in them -- axis
// labels, variable names, the p-values on the permutation plots -- is real text
// rather than glyph outlines. But pkgdown embeds figures as <img src="...svg">,
// and a browser will not let you select, search or copy text inside an <img>.
//
// Swapping each <img class="r-plt"> for the SVG document it points at puts that
// text in the page itself, where Ctrl-F finds it, a mouse can select it and a
// screen reader can read it. Anything that fails to load is simply left alone,
// so the worst case is the plot we would have had anyway.

(function () {
  "use strict";

  function inline(img) {
    var src = img.getAttribute("src");
    if (!src || !/\.svg$/i.test(src)) {
      return;
    }

    fetch(img.src)
      .then(function (response) {
        if (!response.ok) {
          throw new Error(response.status + " for " + src);
        }
        return response.text();
      })
      .then(function (text) {
        var doc = new DOMParser().parseFromString(text, "image/svg+xml");
        var svg = doc.querySelector("svg");
        if (!svg || doc.querySelector("parsererror")) {
          throw new Error("not parseable as SVG: " + src);
        }

        // The viewBox carries the aspect ratio, so the SVG's own pt-based width
        // and height can go. What must survive is the <img>'s sizing: knitr puts
        // the chunk's out.width there (49% for the side-by-side comparisons), and
        // dropping it would blow every figure up to full column width.
        svg.removeAttribute("width");
        svg.removeAttribute("height");
        svg.setAttribute("class", img.getAttribute("class") || "");
        svg.setAttribute("style", img.getAttribute("style") || "");
        svg.style.width = img.getAttribute("width") || "100%";
        svg.style.height = "auto";
        svg.setAttribute("role", "img");

        var alt = img.getAttribute("alt");
        if (alt) {
          var title = doc.createElementNS("http://www.w3.org/2000/svg", "title");
          title.textContent = alt;
          svg.insertBefore(title, svg.firstChild);
        }

        img.replaceWith(svg);
      })
      .catch(function () {
        // Leave the <img> in place: a figure whose text cannot be selected is
        // still a figure.
      });
  }

  function inlineAll() {
    var imgs = document.querySelectorAll("img.r-plt[src$='.svg']");
    Array.prototype.forEach.call(imgs, inline);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", inlineAll);
  } else {
    inlineAll();
  }
})();
