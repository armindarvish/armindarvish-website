---
title: "Improve your mu4e workflow with consult-mu"
author: ["Armin Darvish"]
date: 2024-03-08T09:48:00-08:00
lastmod: 2024-03-08T15:32:30-08:00
draft: false
weight: 3009
subtitle: "get dynamically updated search results, fast async search, interactive menus for attachments, and more..."
summary: "In this post I show you my consultmu package that can add some extra features to your mu4e workflow"
authors:
  - admin
projects: [software]
categories: [software, emacs, email client, mu4e]
featured: false
commentable: true
image:
  caption:
  focal_point: 'center'
---

## Introduction {#introduction}

I have been using `mu4e`, as my email client inside emacs for a few years and in my experience it is one of the best clients because it is feature-rich, and yet distraction-free. I think most features that one would expect in a modern email client is already built-in. You can easily set up multiple accounts using `mu4e-contexts`. The search performance is relatively fast and There is built-in saved searches with `mu4e-bookmarks`. The `mu4e-view-mode` provides one of the best reading panes of any client. You can have it side-by-side or in full window and can quickly navigate through your mailbox from within the reading pane by using `mu4e-view-headers-next` and `mu4e-view-headers-prev`. You can quickly organize emails with `mu4e-marks` from either  `mu4e-headers-mode` and `mu4e-view-mode` (a.k.a. reading pane), and being in emacs everything can be done with strokes of a few keys, which makes email management very efficient. However, there are some areas where I think mu4e can use improvements (at least for my work flow, it does) especially for search UI and performance.

Looking at discussion threads online, there are many posts on `"mu4e v.s. x"` package, and often times there are complains about performance with suggestions to switch to another client/package. For example, in recent years, notmuch has become a popular alternative for its performance and ability to handle many thousands of emails. If you are interested to know why I prefer to use mu4e, you can jump ahead and read the part [at the end of this post](#thoughts-on-mu4e-and-consult-mu-vs-other-tools). But, I decided making a package, [consult-mu](https://github.com/armindarvish/consult-mu), to deal with some of the missing features for my work flow in `mu4e` is a better path than switching entirely to another tool. In this post I am going to summarize some of the important features of `consult-mu` for other people that might be interested and looking for a similar tool. Feel free to explore the tool for yourself and contact me if you have suggestions or feedback.

I should also add that [consult-mu](https://github.com/armindarvish/consult-mu) was highly inspired by [counsel-mu](https://github.com/seanfarley/counsel-mu) and [consult-notmuch](https://github.com/emacsmirror/consult-notmuch), and some other packages like [doomemacs](https://github.com/doomemacs/doomemacs). In a sense, I have tried to combine all the best features I liked in those packages in one place for the specific case of `mu4e` and decided to put it all together in one package that also provides some consistency with my other work flows (for example looking at GitHub repos with [consult-gh](https://github.com/armindarvish/consult-gh)). At the end, I am very happy with how this package has transformed my workflow, plus the fact that the performance (at least in my test) is much better than any other tool I have tried.


## consult-mu {#consult-mu}

The package consult-mu aims to provide some the missing elements especially with search UI from mu4e. These include:


### search {#search}

mu4e search functionality is great. It allows the user to search in different fields and allows modifying search queries with `mu4e-search-edit` command as well as toggling search properties with `mu4e-search-toggle-property` command. But, the search results are not updated dynamically (which is the default experience in most clients nowadays). In addition, if there are thousands of hits for the query, it can get really sluggish and laggy (though this has now been improved by a lot in the latest version 1.12). In contrast, running the same search in the terminal with the underlying mu server (e.g. `mu find`) is actually very fast. This means that it is possible to use mu4e and get very fast search results if we bypass all the elisp front-end. The problem is bypassing the-front end functionalities can reduce the usability as well so at the end one has to find the right balance. Personally, I ended up choosing a mixed approach with two different interactive commands:

-   consult-mu-dynamic

This interactive command allows the user to search emails similar to mu4e-search but the results are dynamically updated in the minibuffer as the search query is being typed. It uses a somewhat hacked version of mu4e-search without taking over buffers and windows like mu4e-search does. More importantly, it allows passing command line arguments to the search therefore all the toggles can be done dynamically within the same search function. For example, if you want to reorder the results by a different field, you can just pass `-z` s an option to the search query and see the results dynamically update.

In terms of performance, this is similar to mu4e-search, and therefore if your search returns thousands of hits, it can be slow because under the hood we are using the same mechanics. In fact, once the user selects a candidate, it opens a `mu4e-header` buffer with all the results similar to running mu4e-search. The advantage however is that the dynamic update provides a nice modern intuitive interface with previews in buffers that stay out of your way if you just want to see an email and go back to whatever you were doing, so it can boost efficiency compared to using mu4e-search.

{{< figure src="/ox-hugo/consult-mu4.gif" width="800px" height="nilpx" >}}

-   consult-mu-async

This interactive command takes a completely different approach. Instead of using the built-in mu4e-search functionality, it uses the `mu find` command in the terminal similar to other async consult commands (e.g. consult-ripgrep) and therefore is much faster than mu4e-search and because the search runs asynchronously, it does not freeze the Emacs instance. However, because the mu4e-search is bypassed, some functionalities are limited. For example, once the user selects a hit, only a single email (or a single thread) is shown in the `mu4e-header` buffer and to go back to the full list of search hits, the user has to run the async search again. The approach here is very similar to consult-notmuch but with the advantage that for the search hit (single message or thread), you will still have all the mu4e functionalities in place and therefore you can just use mu4e as you normally would.

The screenshot below shows how `consult-mu-async` can load over 12000 emails very quickly and provide preview buffers as messages are loading. This is similar to `consult-notmuch` functionality but as you can see below `consult-mu` is much faster than `consult-notmuch`. In my tests (only tested with mu4e versions &gt;1.10), consult-mu consistently performs faster than consult-notmuch.

{{< figure src="/ox-hugo/consult-mu5.gif" width="800px" height="nilpx" >}}

-   consult-mu wrapper

consult-mu package further provides an interactive command called `consult-mu` that calls the variable `consult-mu-default-command`. The idea is that the user can pick either of the two commands above or a user-defined function that build on top of the commands above (e.g. "if condition 1 use dynamic, if condition 2 use async") for common daily use. Personally, I use this to call `consult-mu-dynamic` for most of my everyday use, especially because the `mu4e` built-in search is much snappier with the latest version 1.12 released recently. I then use the async version for cases where I want to search through many emails to find a single email/thread (e.g. a needle in a haystack cases), when I know I won't need the mu4e buffers and other functionalities.


### contacts {#contacts}

Another lacking feature in mu4e is easy access to list of contacts or searching contacts. By default, mu4e does not keep a list of contacts but rather builds the list dynamically by querying the database. Therefore, as far as I know, to find emails from a specific contact, one needs to run a search query like "From:contact@armindarvish.com". This is extremely counter-intuitive considering that every other email client has a way of searching contacts. If I want to find out the contact information of "John Doe", I should not need to search and see all the email correspondence from/to him. There are perhaps other ways to do this like composing a new blank email and use completion framework (corfu, company ,etc.) in the recipient field but this is even more counter-intuitive. There are other packages like org-contacts, etc. that can be used but then one has to make sure that the database for those packages are up to date and synced on every machine. I think the idea of making a contact list dynamically from messages makes a lot of sense (although it can sometime cause seeing invalid emails), but ideally mu4e would have provided easy commands to see a list of contacts, etc.

consult-mu provides an interactive command, consult-mu-contacts, that provides an intuitive contacts search functionality. It uses "mu cfind" shell command and provides a dynamically updated list of results. The advantage of dynamically updating results is that you do not need to remember how to spell someone's name. If you are not sure whether to use "jon" or "john" you can just type "jo", see the result and decide what to do next as you type. Furthermore, consult-mu-contacts-embark provides functions to call embark actions, for example to see a list of all th messages from the contact or to compose an email to the contact, ...

The screenshot below shows searching contacts, grouping the results based on the "domain" of the email, and then loading all the emails from the recipient using `consult-mu`.

{{< figure src="/ox-hugo/consult-mu6.gif" width="800px" height="nilpx" >}}


### attachments {#attachments}

Attachments are another area where mu4e (and other mail composing modes for that matter) lack some intuitive interactive features. For example, in mu4e-compose mode, by default, `C-c C-a` is bound to `mml-attach-file` which allows you to pick a single file to attach but for each file you have to go through a series of questions to confirm attachment. Other attachment commands like `mail-add-attach` don't ask questions but still only allow picking a single file. In any other modern email client you can pick multiple files to attach. `consult-mu-compose-attach` at least partially addresses this. It allows you to select multiple files by using `embark dwim` or if you prefer by jumping to dired buffer and marking multiple files similar to what Doom emacs does. Similarly, `consult-mu-compose-dettach` allows you to quickly remove attached files in an interactive way.

Here is a screenshot showing some of the functionalities. Depending on the major-mode of the current buffer, consult-mu guesses what you want to do. In a compose buffer, it assumes that the user wants to attach files to the current message and uses minibuffer to query the user to select files. In a dired buffer, it assumes that the user wants to attach marked files (or the file at point) to a message and uses minibuffer for the user to select an open compose buffer or a new one. In any other buffer, it first asks th user to select some files, and then a message buffer. There are customization variables to change the default behavior as you wish. Refer to the [the GitHub Repo](https://github.com/armindarvish/consult-mu) and the elisp documentation for further details.

{{< figure src="/ox-hugo/consult-mu-compose.gif" width="800px" height="nilpx" >}}


## Thoughts on mu4e and consult-mu vs other tools {#thoughts-on-mu4e-and-consult-mu-vs-other-tools}


### why use emacs? {#why-use-emacs}

One general questions that you may now ask, is "Why in 2024, would/should one even use mu4e?" There are many articles and blog posts on that topic so I won't get into all the details here, but I try to put some of my thoughts on the topic here for context;

Most modern mail clients (thunderbird, outlook, apple mail, ...) are trying to do many things at the same time. Open any of those apps and you will see many panes. One for a list of emails, another for previews, one pane for a calendar, some tabs for todo tasks, etc. and since they are developed by big teams of professional software developers, they are shiny and polished too. But they are not really that good at their main job, going through lots of emails and doing things on each (delete, reply, archive, forward, ...). In fact, if you have a large pile of emails in your inbox, going through them and quickly sorting them, and doing actions on them is really not that efficient in most of those clients. For one, they tend to load 50-100 emails per page, and while one can scroll or click through pages, the interface is really not optimized for organizing thousands of emails efficiently. Of course, there is often the option of setting key shortcuts and whatnot but at the end the interface is designed for general application to serve many customers rather than optimizing your work flow. In this sense, small packages in emacs (or some command line tool for that matter) will always have the advantage over those big shiny desktop apps, because they can be extremely optimized for just one specific task and since most open source software are not about selling the product to the masses, there is rarely any need to bloat everything just to cater to more customers.

another good example of this is the search functionalities in many modern email clients. While most email clients have a way of allowing you to do some advanced search, most people don't ever use those features and stick to just some simple search terms. This is because, the advanced search functionalities are often hidden in some menu/pages, where you have to fill a form to define search queries, or need learning a whole new query syntax to narrow down your search. In emacs, on the other hand, you can use the same syntax and commands that you would use to search for things in any other buffer (code, text, terminal, ...) to search through your email buffer (e.g. `mu4e-header-buffer`), therefore once you learn the basics of navigation and commands in emacs, you can have all sorts of work flows with the same consistent commands.


### why mu4e instead of other emacs packages (looking at you notmuch)? {#why-mu4e-instead-of-other-emacs-packages--looking-at-you-notmuch}

Specifically in case of `mu4e`, the simplicity of going through many emails and sorting them with simple strokes of single keys, makes it extremely efficient for dealing with lots of emails. But this is not unique to `mu4e`, other tools like [notmuch](https://github.com/notmuch/notmuch) (in combination with [consult-notmuch](https://github.com/emacsmirror/consult-notmuch)) can be used for similar work flows. However, when I tried some other tools, I was always missing some features and functionalities that I feel are necessary in 2024. For example, the ability to render HTML messages is necessary because many emails I receive are htmls. With mu4e, I can use the webkit (at least on macOS) to read these emails. Being able to easily and quickly switch between different accounts is another feature that mu4e handles pretty well with `mu4e-contexts`. More importantly, it's relatively easy to integrate what I do in `mu4e` with what I do elsewhere, fr example when I read an email on my mobile phone. With a package like `notmuch` it was much harder to keep everything in sync.

That said, there are areas where notmuch's minimalist approach (especially when combined with the power of consult and embark) is very appealing, and this is why it made sense to create [consult-mu](https://github.com/armindarvish/consult-mu). For example, running `M-x mu4e` opens a main dashboard-like buffer. This is somewhat similar to switching to a different desktop app for reading emails. It may make sense for the time when one sits down to go through her/his mailbox, but what if I am in the middle of writing some code and I just want to jump to a specific email to copy something into my code? Of course, one can run `M-x mu4e-search` instead of going to the dashboard but lack of dynamic completion with search results, means that you have to first run a search query then see the results in a `mu4e-header` buffer with all the hits and then try to find the one email you are looking for. The `consult-notmuch` approach with doing all the search and dynamic completion in minibuffer and showing previews is much superior in this case. `consult-mu` is exactly trying to enable a similar workflow but with mu4e and all its bells and whistles instead of `notmuch` .
