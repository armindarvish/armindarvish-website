---
# An instance of the Contact widget.
widget: contact

# This file represents a page section.
headless: true

# Order that this section appears on the page.
weight: 130

title: Contact
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
  address:
    street: 384 Santa Trinita Ave.
    city: Sunnyvale
    region: CA
    postcode: '94085'
    country: United States
    country_code: US
  coordinates:
    latitude: '37.382147'
    longitude: '-121.999532'
  contact_links:
  - icon: linkedin
    icon_pack: fab
    name: Linkedin
    link: 'https://www.linkedin.com/in/adarvish/'
  - icon: twitter
    icon_pack: fab
    name: Twitter
    link: 'https://twitter.com/ArIVIiIV'

design:
  columns: '2'
---
