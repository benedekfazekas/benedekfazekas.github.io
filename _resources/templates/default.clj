;;(doctype :xhtml-transitional)
[:html
 {:xmlns "http://www.w3.org/1999/xhtml", :lang "en", :xml:lang "en"}
 [:head
  [:meta
   {:http-equiv "content-type", :content "text/html; charset=UTF-8"}]
  [:meta {:name "description", :content (:description metadata)}]
  [:meta {:name "keywords", :content (:tags metadata)}]
  [:meta {:name "author", :content "Benedek Fazekas"}]
  [:link {:rel "stylesheet" :type "text/css" :href "//cdnjs.cloudflare.com/ajax/libs/font-awesome/3.2.1/css/font-awesome.min.css"}]
  [:link {:rel "stylesheet" :type "text/css" :href "//fonts.googleapis.com/css?family=Source+Code+Pro|Open+Sans"}]
  [:link {:rel "stylesheet", :type "text/css", :href "/bootstrap.min.css"}]
  [:link {:rel "stylesheet", :type "text/css", :href "/custom.css"}]

  [:title (:title metadata)]]
 [:body
  [:div.content
   [:div.container
    [:div.row
     [:div.col-md-8
      (when (or (= (:type metadata) :post)
              (= (:type metadata) :site))
        [:div.page-header
         [:h1 (:title metadata)]])
      [:div.entry
       content]]
     [:div.col-md-4]

     (if (= (:type metadata) :post)
       (reduce
     	(fn[h v]
     	  (conj h [:a {:href (str "/tags/#" v)} (str v " ")]))
     	[:div.col-md-12 {:class "post-tags"} "Tags: "]
     	(.split (:tags metadata) " ")))]]
   [:script {:src "https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js?lang=clj&lang=ruby"}]

   [:div.footer
    [:div.container
     [:div.row
      [:div.col-md-12
       [:p "Built with "
        [:a {:href "http://getbootstrap.com/"} "Bootstrap"] " and "
        [:a {:href "https://github.com/nakkaya/static"} "Static"]
        [:br]
        [:p
         (interleave ["&copy; 2014-2016 Benedek Fazekas"
                      [:a {:href "https://github.com/benedekfazekas"} "@github"]
                      [:a {:href "https://twitter.com/bfazek"} "@twitter"]]
                     (cycle [" | "]))]]]]]]]]]
