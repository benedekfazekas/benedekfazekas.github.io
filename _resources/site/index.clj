{:title "Of Herbs and Stewed Rabbit"}

[:div.container
 [:div.row
  [:div.col-lg-8
   [:div
    (map
     #(let [f %
            url (static.core/post-url f)
            [metadata cont] (static.io/read-doc f)
            content @cont
            date (static.core/parse-date
                  "yyyy-MM-dd" "dd MMM yyyy"
                  (re-find #"\d*-\d*-\d*" (str f)))]
        [:div [:h2 [:a {:href url} (str (:title metadata) "...")]]
         [:p {:class "publish_date"} date]
         (take (+ (.indexOf content "</p>") 4) content)])
     (take 25 (reverse (static.io/list-files :posts))))]
   [:p [:a {:href "/rss-feed"} "RSS feed"]]]
  [:div.col-lg-4]]]
