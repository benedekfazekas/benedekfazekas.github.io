{:title "Benedek's pages"}

[:div.container
 [:div.row
  [:div.col-lg-4
   [:div.list-group
    (map
     #(let [f %
            url (static.core/post-url f)
            [metadata _] (static.io/read-doc f)
            date (static.core/parse-date
                  "yyyy-MM-dd" "dd MMM yyyy"
                  (re-find #"\d*-\d*-\d*" (str f)))]
        [:a {:href url :class "list-group-item"} (str date " -- " (:title metadata))])
     (take 25 (reverse (static.io/list-files :posts))))]]]]
