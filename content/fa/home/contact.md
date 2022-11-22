---
# An instance of the Contact widget.
widget: contact

# This file represents a page section.
headless: true

# Order that this section appears on the page.
weight: 130

title: تماس
subtitle:

content:
  # Automatically link email and phone or display as text?
  autolink: true

  # Email form provider
  form:
    provider: netlify
    formspree:
      id:
    netlify:
      # Enable CAPTCHA challenge to reduce spam?
      captcha: false

  # Contact details (edit or remove options as required)
  email: armindarvish@gmail.com
  contact_links:
  - icon: linkedin
    icon_pack: fab
    name: لینکدین
    link: 'https://www.linkedin.com/in/adarvish/'
  - icon: twitter
    icon_pack: fab
    name: توییتر
    link: 'https://twitter.com/ArIVIiIV'

design:
  columns: '2'
---
