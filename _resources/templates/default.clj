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
     [:div.col-md-12
      (if (or (= (:type metadata) :post)
              (= (:type metadata) :site))
        [:div.page-header
         [:h1 (:title metadata)]])

      content]

     (if (= (:type metadata) :post)
       (reduce
     	(fn[h v]
     	  (conj h [:a {:href (str "/tags/#" v)} (str v " ")]))
     	[:div {:class "post-tags"} "Tags: "]
     	(.split (:tags metadata) " ")))]

    (if (= (:type metadata) :post)
      [:div
       {:id "related"}
       [:h3 {:class "random-posts"} "Random Posts"]
       [:ul
    	{:class "posts"}
	(map
	 #(let [f %
		url (static.core/post-url f)
		[metadata _] (static.io/read-doc f)
		date (static.core/parse-date
		      "yyyy-MM-dd" "dd MMM yyyy"
		      (re-find #"\d*-\d*-\d*" (str f)))]
	   [:li [:span date] [:a {:href url} (:title metadata)]])
	 (take 5 (shuffle (static.io/list-files :posts))))]])]
   [:script {:src "//google-code-prettify.googlecode.com/svn/loader/run_prettify.js"}]
   [:script {:src "//google-code-prettify.googlecode.com/svn/trunk/src/lang-clj.js"}]

   [:div.footer
    [:div.container
     [:div.row
      [:div.col-md-12
       [:p "Built with "
        [:a {:href "http://getbootstrap.com/"} "Bootstrap"] " and "
        [:a {:ref "https://github.com/nakkaya/static"} "Static"]
        [:br]
        [:p "&copy; 2014"]]]]]]]]]
