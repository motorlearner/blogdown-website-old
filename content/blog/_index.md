---

# settings for "blog"

title:                Blog             # this will show up on browser tab
author:               Aslan Bellmann   # 
description:          ""               # 
layout:               list-sidebar     # list, list-sidebar, list-grid
show_author_byline:   true             # show author under blog post entries?
show_post_date:       true             # show date under blog post entries?
show_post_thumbnail:  false            # show tumbnail next to blog post entries?
thumbnail_left:       true             # ...if so, left or right side?
show_button_links:    true             # 

# setings for sidebar

sidebar:
  author:               ""
  title:                Blog | Tales of Uncertainty 
  description:          |
    Welcome to the blog.
    
  show_sidebar_adunit:  true
  text_link_label:      browse categories
  text_link_url:        /categories/

# settings for all sites inside "blog"

cascade:
  author:              Aslan Bellmann  # default blog post author
  show_author_byline:  true            # 
  show_comments:       true            # 
  show_post_date:      true            # 
  sidebar:                             # sidebar params only relevant if "list-sidebar"
    show_sidebar_adunit:  false           # 
    text_link_label:      ""              # can put a link here
    text_link_url:        ""              # underlying link, e.g. /cateogories/

---
