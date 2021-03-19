(ns samcf.docs
  (:require [clojure.data.xml :as xml]
            [clj-http.client :as client]
            [clojure.string :as string]
            [hiccup.core :as hiccup]
            [hiccup.page :as page]
            [markdown.core :as markdown]))

(def pages
  #{"README.md"
    "docs/index.html"
    "docs/posts/%s.html"
    "docs/source.html"
    "docs/feed.xml"})

(defn gists [username]
  (-> (format "https://api.github.com/users/%s/gists" username)
      (client/get {:as :json})
      (:body)))

(defn fetch [& entries]
  (->> entries
       (mapv (fn [post] (future [post (-> post :url client/get :body)])))
       (mapv deref)
       (reduce
        (fn [m v]
          (let [[p c] v]
            (update m (:type p) conj v))) {})))

(def gist->entry
  (comp (map (fn [data] [data (-> data :files vals first)]))
        (filter (fn [[data file]] (not (nil? file))))
        (filter (fn [[data file]] (string/includes? (:filename file) ".site.")))
        (map (fn [[data file]]
               (let [[name _ type ext] (string/split (:filename file) #"\.")]
                 {:name    name
                  :type    (keyword type)
                  :desc    (:description data)
                  :created (:created_at data)
                  :updated (:updated_at data)
                  :url     (:raw_url file)
                  :ext     ext})))))

(defn link-to [url & {:keys [text title external]}]
  [:a {:href url
       :title title
       :target (when external "_blank")}
   (or text url)])

(defn format-date
  ([s] (format-date s "MMM d, yyyy"))
  ([s format] (->> (java.time.Instant/parse s)
                   (java.util.Date/from)
                   (.format (java.text.SimpleDateFormat. format)))))

(defn layout [content & {:keys [title desc]}]
  (hiccup/html
   (page/doctype :html5)
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width"}]
    [:meta {:name "description" :content desc}]
    [:title title " &mdash; " "samcf.me"]
    (for [name #{"reset" "style" "markdown"}]
      [:style (slurp (format "docs/assets/%s.css" name))])
    [:style (slurp "docs/assets/highlight/github-gist.css")]
    (page/include-js "/assets/highlight/highlight.pack.js")
    [:script "hljs.highlightAll();"]]
   [:body
    [:div.root
     [:header
      (link-to "/" :text "samcf.me" :title "Navigate to the home page")
      (link-to
       "//github.com/samcf"
       :text "github.com/samcf"
       :title "Check out my projects on Github"
       :external true)
      (link-to "mail@samcf.me" :title "Send me an email")
      (link-to
       "/source.html"
       :text "source.html"
       :title "Navigate to this project's source code")
      (link-to
       "/feed.xml"
       :text "feed.xml"
       :title "RSS feed for the blog posts")]
     content
     [:footer "Sam Ferrell 2021"]]]))

(defmulti render (fn [filename _] filename))

(defmethod render "README.md" [filename gists]
  (->>
   (for [[post _] (:post gists)]
     (format "- %s [%s](//samcf.me/posts/%s.html)"
             (format-date (:created post))
             (:desc post)
             (:name post)))
   (apply
    str
    (str
     "## Welcome!\n"
     "My name is **Sam Ferrell**; I'm a software engineer specializing in user"
     " interfaces, browser applications, relational data modeling, and board"
     " game programming. Experience with small team management, product"
     " planning, and engineering processes. Interested in focused teams and"
     " social missions for underserved domains and populations. Very familiar"
     " with TypeScript, SQL, React, Elixir, and Clojure.\n\n"
     "## Posts\n"))
   (vector filename)
   (vector)))

(defmethod render "docs/index.html" [filename gists]
  [[filename
    (layout
     (let [[[_ about]] (render "README.md" gists)]
       [:div.about.markdown
        (markdown/md-to-html-string about)])
     :title "Home"
     :desc (str "Software engineer specializing in user interfaces, browser"
                " applications, relational data modeling, and board game"
                " programming."))]])

(defmethod render "docs/posts/%s.html" [filename gists]
  (for [[post content] (:post gists)]
    [(format filename (:name post))
     (layout
      [:div.markdown
       [:p (format-date (:created post))]
       (markdown/md-to-html-string content)]
      :title (:desc post)
      :desc (:desc post))]))

(defmethod render "docs/source.html" [filename _]
  [[filename
    (layout
     [:div.markdown
      [:h2 "Source Code"]
      (str "The following Clojure is responsible for generating the content"
           " on this site. The content is primarily driven by the public"
           " gists on ")
      (link-to
       "//gist.github.com/samcf"
       :text "gist.github.com/samcf"
       :title "Public gists of Sam Ferrell"
       :external true)
      ". The project itself is hosted here "
      (link-to
       "//github.com/samcf/samcf.github.io"
       :text "github.com/samcf/samcf"
       :title "Navigate to the project repo"
       :external true)
      "."
      [:br]
      [:br]
      [:pre [:code (hiccup.util/escape-html (slurp "src/samcf/docs.clj"))]]]
     :title "Source code"
     :desc (str "The Clojure implementation of my static site generator which"
                " created this page."))]])

(defmethod render "docs/feed.xml" [filename gists]
  (->> [:rss {:version "2.0"}
        [:channel
         [:title "Sam Ferrell's Thoughts"]
         [:description "Writing and thoughts"]
         [:link "https://samcf.me"]
         [:copyright "Sam Ferrell 2021"]
         [:language "en-us"]
         (for [post (:post gists) :let [[post content] post]]
           [:item
            [:link (format "https://samcf.me/posts/%s.html" (:name post))]
            [:title (:desc post)]
            [:desc (:desc post)]
            [:pubDate (format-date (:created post) "E, d MMM y k:m:s Z")]])]]
       (xml/sexp-as-element)
       (xml/emit-str)
       (vector filename)
       (vector)))

(defn -main []
  (let [gists (->> (gists "samcf") (into [] gist->entry) (apply fetch))
        files (->> pages (mapcat #(render % gists)))]
    (doseq [[filename content] files]
      (spit filename content))))
