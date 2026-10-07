; The default page shell. It owns the whole document: Html hands it the
; finished page body as `body` and the page metadata as `page`. Fork it
; with `theme.layout: <path under src/>` in config.yaml.
;
; Context: site, page, content, nav-links, footer-links, cssRef, config,
; posts, pages, tags, data. Helpers: og-tags, math-tags, theme-js,
; code-js, t.

(defn link-item (l)
  (a {:href (get l "href")} (get l "label")))

(defn footer-seq (links sep)
  (if (== (len links) 0)
    nil
    (if (== (len links) 1)
      (link-item (first links))
      [(link-item (first links)) sep (footer-seq (drop links 1) sep)])))

[
  (raw "<!DOCTYPE html>")
  (html {:lang (get site "siteLang")}
    (head
      (meta {:charset "utf-8"})
      (meta {:name "viewport" :content "width=device-width, initial-scale=1, viewport-fit=cover"})
      (meta {:name "description" :content (get site "siteDescription")})
      (title (get page "title"))
      (og-tags page)
      (math-tags page)
      (map (get (get config "theme") "extraJs")
           (fn (f) (script {:defer true :src (str "/" f)})))
      (theme-js)
      (code-js)
      (link {:rel "stylesheet" :href cssRef}))
    (body
      (header {:class "site-header"}
        (nav
          (a {:class "site-name" :href "/"} (get site "siteName"))
          (map nav-links link-item)))
      (main content)
      (footer {:class "site-footer"}
        (nav {:class "footer-links"} (footer-seq footer-links (get site "footerSeparator")))
        (p
          (get site "siteCopyright")
          (if (get site "siteGeneratedBy")
            (str " · " (get site "siteGeneratedBy"))
            nil))
        (button {:class "theme-toggle" :type "button" :aria-label (t "themeToggle")}))))
]
