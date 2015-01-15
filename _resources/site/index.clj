{:title "Of Herbs and Stewed Rabbit"}

[:div.container
 [:div.row
  [:div.col-lg-12
   [:div
    (map
     #(let [f %
            url (static.core/post-url f)
            [metadata _] (static.io/read-doc f)
            date (static.core/parse-date
                  "yyyy-MM-dd" "dd MMM yyyy"
                  (re-find #"\d*-\d*-\d*" (str f)))]
        [:div [:a {:href url} (str date " -- " (:title metadata) "...")]])
     (take 25 (reverse (static.io/list-files :posts))))]
   [:p [:a {:href "/rss-feed"} "RSS feed"]]]]]
