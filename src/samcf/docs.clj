(ns samcf.docs
  (:require [clj-http.client :as client]
            [clojure.core.async :as async]
            [clojure.string :as string]
            [hiccup.core :as hiccup]
            [hiccup.page :as page]
            [markdown.core :as markdown]))

(def pages
  #{"docs/index.html"
    "docs/posts/%s.html"
    "docs/source.html"})

(defn gists [username]
  (-> (format "https://api.github.com/users/%s/gists" username)
      (client/get {:as :json})
      (:body)))

(defn fetch [& entries]
  (->> entries
       (mapv
        (fn [post] (async/go [post (-> post :url client/get :body)])))
       (async/merge)
       (async/reduce
        (fn [m v]
          (let [[p c] v]
            (update m (:type p) conj v))) {})
       (async/<!!)))

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

(defn format-date [s]
  (->> (java.time.Instant/parse s)
       (java.util.Date/from)
       (.format (java.text.SimpleDateFormat. "MMM d, yyyy"))))

(defn layout [content & {:keys [title desc]}]
  (hiccup/html
   (page/doctype :html5)
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width"}]
    [:meta {:name "description" :content desc}]
    [:title title " &mdash; " "samcf.me"]
    (for [name #{"reset" "style" "markdown"}]
      [:style (slurp (format "docs/assets/%s.css" name))])]
   [:body
    [:div.root
     [:header
      (link-to "/" :text "samcf.me" :title "Navigate to the home page")
      (link-to
       "//github.com/samcf"
       :text "github.com/samcf"
       :title "Check out my projects on Github"
       :external true)
      (link-to "mailto:mail@samcf.me" :title "Send me an email")
      (link-to "source.html" :title "Navigate to this project's source code")
      [:address "denver, co"]]
     content]]))

(defmulti render (fn [filename _] filename))

(defmethod render "docs/index.html" [filename gists]
  [[filename
    (layout
     [:div
      [:div.about.markdown
       (let [[[_ content]] (:about gists)]
         (markdown/md-to-html-string content))]
      [:div.posts.markdown
       [:h1 "Posts"]
       (for [[post _] (:post gists)]
         [:div
          (format-date (:created post)) " "
          (link-to (format "/posts/%s.html" (:name post)) :text (:desc post))])]]
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
      [:p (str "The following Clojure is responsible for generating the content"
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
        :text "github.com/samcf/samcf.github.io"
        :title "Navigate to the project repo"
        :external true)
       "."]
      [:pre [:code (hiccup.util/escape-html (slurp "src/samcf/docs.clj"))]]]
     :title "Source code"
     :desc (str "The Clojure implementation of my static site generator which"
                " created this page."))]])

(defn -main []
  (let [gists (->> (gists "samcf") (into [] gist->entry) (apply fetch))
        files (->> pages (mapcat #(render % gists)))]
    (doseq [[filename content] files]
      (spit filename content))))
