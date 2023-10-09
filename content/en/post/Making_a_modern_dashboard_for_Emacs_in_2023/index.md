---
title: "Making a modern dashboard for Emacs in 2023"
author: ["Armin Darvish"]
lastmod: 2023-09-06T17:55:08-07:00
draft: true
weight: 3007
subtitle: "A Tutorial on How  to Create a Modern, Functional Dashboard to Orgzanize your Workfows in Vanilla Emacs"
summary: "In this post I share the config for my Emacs' dashboard after many people have requested it since my previous posts."
authors:
  - admin
projects: [software]
categories: [software, emacs, dashboard]
featured: false
commentable: true
image:
  caption:
  focal_point: 'center'
gallery_item:
- album: emacs-dashboard-logos
  image: armindarvish-bird.png
  caption: A bird logo for your emacs dashboard. Good for both dark and light themes.
- album: emacs-dashboard-logos
  image: armindarvish-eagle.png
  caption: An eagle logo for your emacs dashboard. Good for dark themes only.
- album: emacs-dashboard-logos
  image: armindarvish-elogo1.png
  caption: A modern logo for your emacs dashboard. Good for light themes only.
- album: emacs-dashboard-logos
  image: armindarvish-elogo2.png
  caption: A modern logo for your emacs dashboard. Good for light themes only.
- album: emacs-dashboard-logos
  image: armindarvish-elogo3.png
  caption: A modern logo for your emacs dashboard. Good for both dark and light themes.
- album: emacs-dashboard-logos
  image: armindarvish-elogo4.png
  caption: A modern logo for your emacs dashboard. Better for light themes.
- album: emacs-dashboard-logos
  image: armindarvish-elogo5.png
  caption: A modern logo for your emacs dashboard. Good for both dark and light themes.
- album: emacs-dashboard-logos
  image: armindarvish-gnu1.png
  caption: A gnu logo for your emacs dashboard. Good for both dark and light themes.
- album: emacs-dashboard-logos
  image: armindarvish-gnu2.png
  caption: A gnu logo for your emacs dashboard. Good for both dark and light themes.
- album: emacs-dashboard-logos
  image: armindarvish-gnu3.png
  caption: A gnu logo for your emacs dashboard. Good for dark themes only.
- album: emacs-dashboard-logos
  image: armindarvish-lion.png
  caption: A lion logo for your emacs dashboard. Better for dark themes.
- album: emacs-dashboard-logos
  image: armindarvish-octopus.png
  caption: An octopus logo for your emacs dashboard. Good for both dark and light themes.
---

Ever since I shared some of my emacs workflows and packages on this blog, I have been getting requests for my Dashboard config, and in this post I am finally going to write about it. Now, I should warn you that I am not going to share a config you can copy paste in your init file and use as is. If you are looking for one of those there are enough blog posts, youtube videos, GitHub gists, etc by people who are much better than me at creating such content. What I like to write about in this blog is integrating different tools into a practical and efficient workflow. What does that mean? It means that instead of showing how to configure one package, I tend to focus on how to combine different packages into a workflow. For example in my post about using emacs as a blog editor, [Building the Best Blog Editor with Emacs Org-Mode in 2023?](https://www.armindarvish.com/en/post/building_an_efficient_blogging_workflow_in_emacs/), I wrote about how to combine `org-todo` keywords with `ox-hugo` and some other tools like `org-capture` templates and `yasnippet` , ... to create a fully functional and efficient blog editor inside emacs. In this post I am going to try to do something similar with the Dashbaord! I am going to show you how you can integrate the Dashboard as part of your custom workflow. In the process of showing you how to make a more modern dashboard personalized to your own taste, I will also share some tips for.
for better look and feels if that's what you are looking for.


## Before Getting Started {#before-getting-started}


### Ask yourself why you should make a dashboard and is it worth the time? {#ask-yourself-why-you-should-make-a-dashboard-and-is-it-worth-the-time}

Before we start talking about creating a dashboard, take a step back and ask yourself if this is useful to you? and how would it improve your day-to-day workflow. Personally, the Dashboard is a great tool for me to see an overview/summary of what I am currently working on (different projects, blog posts, ...) and choose the next task. It helps me stay focused on the tasks that are of priority and organize my workflow around them. I can of course use an org file with tasks in it to organize everything I do, but whenever I tried something like that it grew and became very messy quickly and then it required too much overhead to maintain and eventually was abandoned. The dashboard, on the other hand keeps things simple and clean and cannot be too crowded and as a result helps me focus on what matters the most. I still use org-files for keeping all the details but I don't have to worry about maintaining them and keeping them clean as much anymore. In other words, a good dashboard keeps all the noise out of my immediate view and directs me to all the important signal. This has helped me stay away from the "never-ending spiral of tinkering with Emacs configs". I start my emacs in my dashboard and right away I can see the tasks/projects/... I am working on and instead of wandering to other distracting ideas, I will directly jump to important items I have to work on. Over time, I have made a habit of always returning to the dashboard, and look at the overview again before picking the next item and as a result I stay focused on getting things done.

Now, if you look at other dashboards like the default dashboard in doom emacs or spacemacs, you may be thinking that this is pointless. After all, you can simply open any of those links in the dashboard by opening a new buffer anytime needed from any buffer and can quickly switch between them between them (e.g. by `C-x b`) as well. If you use a package like [consult](https://github.com/minad/consult), you can even further customize buffer switching and see items such as your recent files, ... when switching buffers. So what is the purpose of the dashboard then? Is it just a fancy-looking starting buffer to replace scratch-buffer? The answer depends of course on how you use Emacs. Personally I tend to open a lot of buffers, just like I do with browser tabs, and while I can do `M-x consult-buffer` and search through them to find the ones I am looking for, it does not provide an overview/summary like my dashboard (items in my agenda, projects I am working on, etc.). In other words, if have a Dashboard that simply contains a bunch of static shortcuts to some files or buffers (like the default dashboard in doom emacs), then there is not much of a use to it. You can directly jump to those files/buffers without going to dashboard first. But, if you make a custom dashboard that is more dynamic and organizes tasks, projects, and etc. for you and gives you a high-level overview of the most important items, then it becomes much more useful and you will likely find yourself going back to it.

The other question you may want to ask yourself is: "Is it worth it to spend time to make a custom dashboard? It is after all yet another tinkering task that keeps you from doing actual work!". In general, this is a question a lot of people struggle with when it comes to configuring Emacs. If you are one of those who constantly find yourself in this trap of tinkering with Emacs config instead of doing actual work, then remember this is not so much of an "Emacs problem" but more of  a "YOU problem". You need to change the way you approach using Emacs and building your config, etc. I don't intend to blame you for anything. A lot of emacs users at some point or another fall into this trap and go down the rabbit hole of configuring hundreds of packages only to realize that they don't use 90% of them. I certainly have gone down that rabbit hole before and quite ironically the answer is well some Emacs package 😆 (including the Dashboard I show you in this post).Here is generally what as helped me to focus on getting things done rather than constantly changing my Emacs config. Instead of focusing on configuring a single package based on some shiny videos I just watched online, I focus on building workflows for current projects I am working on. I break work packages into smaller meaningful blocks of simple tasks. Then I find the right packages that fits my workflow and start testing them out with the **bare minimum configuration** until I hit a bottleneck and see the need to change things. Then, with the knowledge of the specific issues I need to solve (rather than imaginary nice-to-have wishes), I spend some time doing research, looking at other people's approaches and optimizing my own config. Through multiple such iterations I arrive at a highly personalized workflow that serves every need I have without spending a lot of time on extra configurations I will never need and it all happens as part of getting other works done and not instead of them.

That is, of course, not to say that I don't randomly explore packages and configs. I definitely do that every now and then to see new tools and approaches that may give me ideas for how to improve my workflow. Sometimes wandering around without any destination or directions in mind is the best way to find the most amazing least-explored destinations after all, but do that too often you will ensure you get lost! There is a balance to keep and trade-offs to make when building your own tools. My suggestion is to always start with the minimal working prototype and refine everything from there as the needs arise. Contrary to the popular beliefs, great products and tools are often the result of many years of evolution and not execution of a well-engineered genius plan! What I am about to show you here is the result of refining my own dashboard over several years, and it will likely go through many more iterations as my projects, workflows and etc. evolve over time. In fact a great advantage of building your own tools in Emacs is exactly the this point that they can evolve with you and transform with your project and your needs rather than by some popular trend or the latest fad around the corner.


### Plan your Dashboard, or the first iteration of it anyway {#plan-your-dashboard-or-the-first-iteration-of-it-anyway}

A dashboard is by nature a personalized/customized tool. It is supposed to present the **most important** information for the context in the **most efficient and concise** form. In a car, the dashboard shows you what is the most important indicators to watch for the task at hand, driving. In Emacs, however, the most important indicators depend on what it is that you are currently doing in Emacs. Before you build your dashboard, then, you need to review how you use Emacs and decide what is the context (use-cases, frequency, importance, ...) and choose the most important information/indicators that you want to see on your dashboard. The good thing about doing things in Emacs is that not only everything is customizable, but they are also dynamic. Your Dashboard can change based on time of the day, or your recent activities, or based on the context of what you're doing in other buffers, etc.

Start by thinking through your workflow and organize what you do in Emacs into meaningful blocks or categories, and then ask yourself what your dashboard should contain for each of these and how should it present it to you. To me, a dashboard is part of a `"system of doing things"`. It focuses on tasks and projects, and organizes them in order of priorities for me. But, to you it might mean something else. May be you want to use it to see "a list of books" you are reading or you want to see "pictures of destinations" you want to visit or may be you want to keep "a list of values" you want to remember or "description of ideas/concepts" you want to think about, ...

One more thing to understand before we start coding is that an Emacs dashboard is by nature only a transient state. It is often just a starting page that you may return to temporarily to see an overview of everything before you jump to the next item. So, at the end of the day, you will only spend a limited amount of time looking at this dashboard, and therefore it should be concise and clear and easy to navigate otherwise it won't be as useful.


### Setting up Emacs {#setting-up-emacs}


#### Setting  packages {#setting-packages}

In my own emacs config, I use [straight.el](https://github.com/radian-software/straight.el) and use a literate config file in org-mode. But for the sake of simplicity and keeping this post beginner-friendly, I am going to show you a minimal approach with built-in `package.el` and `use-package`. If you are an advanced user, and just want to see the dashboard config, jump ahead and look at the dashboard section. You should be able to copy the config for the dashboard package and adapt it with any package manager you use. If you are a beginner, I recommend starting with something simple to learn the basics before switching to more advanced package managements and configuration. The [Emacs From Scratch Series](https://www.youtube.com/playlist?list=PLEoMzSkcN8oNmd98m_6FoaJseUsa6QGm2) by David Wilson is probably a good place to start.

Here is a minimal setup for package management using `use-package`:

```emacs-lisp
(require 'package)

;; Adds the Melpa archive to the list of available repositories
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/"))
      )

;; Initializes the package infrastructure
(package-initialize)
(package-refresh-contents)
(package-install 'use-package) ;;no need to install use-package in Emacs >29 since it is built-in
(setq use-package-always-ensure t)
```


#### General Layout, Font and Theme {#general-layout-font-and-theme}

First things first, consider picking a layout, theme, font, etc. and find some settings you are going to stick with for a while. A dashboard is a place where you prioritize important information, topics, projects, tasks , ... and that often requires ui elements to present information in a concise format. For example using icons, or color-coding can help create patterns which helps building habits (e.g. focus on important info in red color, start with checking items with important icon, etc.) and that leads to an organized efficient workflows over time. Therefore, it is important to decide on the general appearance of your Emacs and find ui elements you like before you spend too much time on your dashboard's looks and feel.

<!--list-separator-->

-  Layout

    Start with different settings for toolbars and menus to find what is most appealing to you. Most advanced users would suggest turning all the toolbars off, since in Emacs you would mostly be using keyboards anyway, but at the beginning you may want to experiment with it and find out what works for you. There are also other universally recommended settings, like turning the bells off!

    ```emacs-lisp
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (tooltip-mode 1)
    (scroll-bar-mode -1)
    (setq inhibit-startup-screen t)
    (setq visible-bell nil)
    ```

<!--list-separator-->

-  Theme and Font

    A quick and simple starting point can be [modus-themes](https://github.com/protesilaos/modus-themes) that comes built-in with newer versions of Emacs. I personally like dark-themes better because they allow me to focus better and therefore would recommend `modus-vivendi` or one of the other dark alternatives. But if you like light themes, `modus-operandi` is also great. If those do not look appealing to you, you can try [ef-themes](https://github.com/protesilaos/ef-themes) by the same author. You can always customize the theme later, so as long as it generally looks good, that should serve fine as a starting point. For fonts, [Fira Code](https://fonts.google.com/specimen/Fira+Code) or [Roboto Mono](https://fonts.google.com/specimen/Roboto+Mono) can be a good starting point.

    ```emacs-lisp
    (load-theme 'modus-vivendi-tinted)
    (set-face-attribute 'default nil :font "Roboto Mono" :height 190)
    ```

    In addition, having icons is useful especially if you later want to use icons in your dashboard as well. [all-the-icons.el](https://github.com/domtronn/all-the-icons.el) has historically been a favorite icon package, but more recently [nerd-icons.el](https://github.com/rainstormstudio/nerd-icons.el) is an alternative that also works in the terminal.

    ```emacs-lisp
    (use-package nerd-icons
      :config
      (setq nerd-icons-font-family "Symbols Nerd Font Mono")
      )
    ```

<!--list-separator-->

-  Modeline

    Next, make sure you chose a good modeline. I personally like [doom-modeline](https://github.com/seagle0128/doom-modeline), but there are other options for configuring modeline.  You can refer to [EmacsWiki's Modeline Configuration](https://www.emacswiki.org/emacs/ModeLineConfiguration) page for some suggestions. If you chose doom-modeline, install nerd-icons first if you want icons. Here is some config suggestion for doom-modeline:

    ```emacs-lisp
    (use-package doom-modeline
      :after nerd-icons
      :init
      (doom-modeline-mode 1)
      (setq doom-modeline-support-imenu t)
      :custom
      (doom-modeline-height 10)
      (doom-modeline-bar-width 6)
      (doom-modeline-icon t)
      (doom-modeline-major-mode-icon t)
      (doom-modeline-major-mode-color-icon t)
      (doom-modeline-minor-modes nil)
      (doom-modeline-buffer-encoding nil)
      (doom-modeline-modal-icon t)
      (doom-modeline-enable-word-count nil)
    )
    ```


#### Consider installing some other packages {#consider-installing-some-other-packages}

Consider using other packages depending on your needs. There are a handful of packages that can be considered essential. These are the ones that will improve your emacs experience including setting up your configuration:

1.  Completion Framework: At some point you would need a completion package. The set of packages: [vertico](https://github.com/minad/vertico), [consult](https://github.com/minad/consult), [marginalia](https://github.com/minad/marginalia), [orderless](https://github.com/minad/orderless), and [embark](https://github.com/oantolin/embark) are great for completion. [ivy](https://github.com/abo-abo/swiper) and [helm](https://github.com/emacs-helm/helm) are other popular alternatives.

2.  Key-binding Setting and Menu: [general.el](https://github.com/noctuid/general.el) is a great package that simplifies your keybindingf config. [which-key](https://github.com/justbur/emacs-which-key) provides useful visual menus if you want reminders of keybindings.

3.  Evil-mode: [evil](https://github.com/emacs-evil/evil) is great if you like vim keybindings.

Importantly, you should think about packages/settings that allow creating better items in your dashboard.


## Making a Modern Dashboard {#making-a-modern-dashboard}

Finally we are ready to dive into building a Dashboard. If you are new to Emacs, I recommend you use emacs for a few weeks or months and experiment with your settings for appearance, keybindings, etc. before thinking about building a dashboard.

For the purpose of this post, I am going to use the fabulous [emacs-dashboard](https://github.com/emacs-dashboard/emacs-dashboard) package as it provides a bunch of useful elements right out of the box. you can install it by:

```emacs-lisp
(use-package dashboard
  :demand t
  :hook
  (dashboard-mode . visual-line-mode)
  :config
  (dashboard-setup-startup-hook)
  (dashboard-open)
  )
```

Out of the box, the Dashboard package provides several general segments but you will likely need to customize it significantly if you want more than just a bunch of static links. I my opinion, the default appearance, is also not that great. So in the rest of this post, I'll show you how to customize everything. The official manual on [emacs-dashboard](https://github.com/emacs-dashboard/emacs-dashboard) contains everything you need to know about the mechanics of customizing the dashboard, but here I am going to try to put things in context and focus on ideas for how to make more dynamic elements and more modern looks, etc.


### Basic Configuration of the Dashboard {#basic-configuration-of-the-dashboard}

Here is an overview of the sections, faces, etc. for reference. The labels that are underscored are the name of `faces` and the rest are the name of variables.

{{< figure src="/ox-hugo/emacs dashboard overview.png" width="800px" height="nilpx" >}}

Note that when you load the dashboard by default some of these sections like the navigator buttons may not be there, but after you configure your dashboard this is pretty much everything you can have, and everything here is customizable.


#### Changing Colors and Faces {#changing-colors-and-faces}

You can change the color of the banner or the text by changing the relevant faces (see the picture above). In the code below I am changing the faces for `dashboard-banner-logo-title` and the `dashboard-text-banner`.

```emacs-lisp
 (set-face-attribute 'dashboard-banner-logo-title
                      nil
                      :foreground "#FF0BAF"
                      :weight 'bold)

(set-face-attribute 'dashboard-text-banner
                      nil
                      :height 1.5
                      :weight 'bold
                      :inherit 'warning)
```

One good way of configuring faces, is to use `:inherit` property and link the faces to other faces that most themes customize like `font-lock` faces. That way, if you switch themes, your dashboard looks changes accordingly. Personally I have my own customized theme and almost never switch to another theme, therefore I directly set the faces to what matches my default theme.


#### Set the Logo {#set-the-logo}

The logo is one of the main features of my own dashboard. I like to have some pictures that blends nicely with the background to make the look appealing to me. This comes at the cost of some real state, so I can see fewer items in my dashboard without scrolling, but I like my dashboard to look nice and inspiring so that every time I open the dashboard it gives me a refreshing energetic feeling for the next task otherwise if it is just a boring list of things to do, I will not be so willing to go back to it. Going back to my dashboard is also a short break time, so I like to have contents that make it a little bit more fun and take my attention away from just getting things done for a brief moment.

For the logo, I usually use some artsy AI-generated colorful photo that I make on [Midjourney](https://www.midjourney.com/). If you like to make your own photos, here are some tips:

1.  Use some color schemes that go well with your theme. For example for dark themes (e.g. modus-vivendi), I like to use neon colors.
2.  I usually use a 3:2 aspect ratio and center the image. On most screens this looks very good. If you want a full-width banner instead, then you can make an image of the exact width of your screen and use that.
3.  Use an image with a transparent background (or start with a solid background and remove the background in a photo-editing app) so it naturally blends in with the background of your emacs.

Here is an example of what I made on [Midjourney](https://www.midjourney.com/home/?callbackUrl=%2Fapp%2F) with a simple prompt like this:

```text
abstract lion logo with dark background, pink blue and orange colors, particles --ar 3:2
```

{{< figure src="/ox-hugo/armindarvish-lion.png" width="nilpx" height="300px" >}}

I have removed the dark background in photoshop. Note how the particle/flame effect makes this blend in with the background and gives the logo some depth.

For a light theme (or in general for something that looks good on both dark and light theme), it might be better to start with a picture with white background and then remove the background. Here is another Midjourney example for a modern emacs logo with **a gnu and the letter e**:

```text
modern minimalist logo for a text editor app called "emacs", purple ink bubble, letter E, shape of a gnu head, geometric, neon colors, futuristic, white background, modern software logo, --ar 3:2
```

{{< figure src="/ox-hugo/armindarvish-gnu.png" width="nilpx" height="300px" >}}

and, here is a full album with more examples for you to get inspiration from:

{{< gallery album="emacs-dashboard-logos" resize_options="300x200" >}}

Download one of the photos you like and set the `dashboard-startup-banner`:

```emacs-lisp
(setq dashboard-startup-banner (expand-file-name "path-to-the-image-file" user-emacs-directory)
```


#### Set Sections {#set-sections}

Let's see how we can toggle certain sections on/off. Generally we will be using the variables `dawshboard-set-*` to turn sections on/off. Here is a script that turns all the sections on.

```emacs-lisp
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-footer t)
(setq dashboard-set-init-info t)
(setq dashboard-set-navigator t)
(setq dashboard-week-agenda t)
```

Note that these are different from `dashboard-*` variables. The former variables are for toggling hide/show whereas the later ones contain the contents or change the style of the text. For example `dashboard-set-footer` is to show/hide the footer whereas the variable `dashboard-footer` contains the string of the current footer. In addition there are some extra variables to further customize the contents. You can read about customizing these variables in the official documents, [here](https://github.com/emacs-dashboard/emacs-dashboard). Just to show you some examples, in the code below I set the variable `dashboard-center-content` to make the text centered and also change the `dashboard-item-names` to change the default title of the `agenda` section:

```emacs-lisp
(setq dashboard-center-content t)
(setq dashboard-item-names '(("Agenda for the coming week:" . "Agenda:")))
```


#### Set the text below the log {#set-the-text-below-the-log}

For the text below the logo, instead of using some fixed text, I like to use something more dynamic. For example, I like to have random inspiring quotes on my dashboard. When I am transitioning from one task to the next, I jump to my dashboard and take a break to read the quote and reflect on it, then I pick up the work for the next task. For  getting quotes, I use the package [motivate](https://github.com/mubaris/motivate). It is a python-based package that gets motivational quotes in the command line. Follow the official instructions to install it. Then you can use `shell-command-to-string` to call it and get a quote as a string. You may then have to do some clean up with `replace-regexp-in-string`. Here is a snippet that gets the quote and cleans it up:

```emacs-lisp
(defun ad/get-motivational-quote ()
(let ((quote (replace-regexp-in-string "\^[.*?m" "" (shell-command-to-string "motivate"))))
(string-match "\"\\(?1:.*\\).*\".*\n.*--\\(?2:.*\\)\n" quote)
(concat "\"" (match-string 1 quote) "\"" "\n\t\t\t\t\t\t'" (match-string 2 quote) "'")))
```

Then you can set the text below the logo like this:

```emacs-lisp
(setq dashboard-banner-logo-title (ad/get-motivational-quote))
```


#### The navigator {#the-navigator}

The navigator is one of the most useful parts in my personal dashboard. I put shortcuts to things that I would like to jump to (e.g. my emacs config, projects, github repos, ...) I like to have a bunch of navigator buttons (some with icons and texts and others with just icons).
The snippet below uses `nerd-icons` and some other packages like my own [consult-gh](http://localhost:1313/en/post/consult-gh_working_with_github_inside_emacs_in_2023_/) to access github projects, [mu4e](https://github.com/djcb/mu) to open emails, [elfeed](https://github.com/skeeto/elfeed) to read feeds, etc. You can make buttons and run any elisp function that you like.

```emacs-lisp
    (setq dashboard-navigator-buttons
        `((;; Line 1

              ;;;;Emacs Config
           (,(nerd-icons-faicon "nf-fa-gear" :height 0.8 :v-adjust 0.0)
            "Settings"
            "Configure Emacs"
            (lambda (&rest _) (find-file
                               (expand-file-name "init.el" user-emacs-directory))) dashboard-heading "" "")
              ;;;;Github
           (,(nerd-icons-octicon "nf-oct-mark_github" :height 0.8 :v-adjust 0.0)
            "Github"
            "Open Github in Browser"
            (lambda (&rest _) (consult-gh-default-repos)) dashboard-heading "" "")

              ;;;;projects
           (,(nerd-icons-mdicon "nf-md-rocket_launch" :height 0.8 :v-adjust 0.0)
            "Projects"
            "Open my projects"
            (lambda (&rest _) (consult-projectile-switch-project)) dashboard-heading "" "")
           )

          (;; line 2
               ;;;;empty space
           ("\t\t\t\t\t\t\t\t\t\t\t\t\t" "" "" nil nil "" "")

               ;;;;emails
           (,(nerd-icons-mdicon "nf-md-email" :height 1 :v-adjust 0.0)
            ""
            "Check Emails"
            (lambda (&rest _) (mu4e)) dashboard-heading "" "")

               ;;;;rss
           (,(nerd-icons-mdicon "nf-md-rss_box" :height 1 :v-adjust 0.0)
            ""
            "Check Feed"
            (lambda (&rest _) (elfeed)) dashboard-heading "" "")

               ;;;;calendar
           (,(nerd-icons-mdicon "nf-md-calendar" :height 1 :v-adjust 0.0)
            ""
            "Calendar"
            (lambda (&rest _) (cfw:open-calendar-buffer)) dashboard-heading "" "")

               ;;;;help
           (,(nerd-icons-mdicon "nf-md-help_rhombus" :height 1 :v-adjust 0.0) "" "?/h" (help) dashboard-heading "" "")
           )))
```

```emacs-lisp
(insert (battery-format "%b%p%% %t hours" (funcall battery-status-function)))
(insert (concat (car doom-modeline--battery-status) (cdr doom-modeline--battery-status)))
```

```emacs-lisp
  (defvar my:motivate-quote (format "%s"
                                    (replace-regexp-in-string "\t" " " ;;replace "\"
                                                              (replace-regexp-in-string "/\\/" "" ;;replace "\" with newline
                                                                                        (replace-regexp-in-string  "\n" "" ; remove all newlines
                                                                                                                   (replace-regexp-in-string "\^[.*?m" ""  ;remove extra garbage
                                                                                                                                             (shell-command-to-string "motivate")))))))


  ;; (setq dashboard-init-info my:motivate-quote)
  (setq dashboard-banner-logo-title my:motivate-quote)
```

```emacs-lisp
(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  (get-buffer-create "*dashboard*")
)
```

```emacs-lisp
(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  (get-buffer-create "*dashboard*")
  :hook
  (dashboard-mode . visual-line-mode)
  :general
  (:keymaps 'dashboard-mode-map
            :states '(normal insert motion visual emacs replace operator)
            "<f5>" 'dashboard-refresh-buffer
            "f" 'find-file
            "m" '(lambda () (interactive) (mu4e-search-bookmark "maildir:/gmail/armindarvish/Inbox/"))
            "c" 'my:calendar
            "e" 'elfeed
            "SPC" '(:ignore t :which-key "jump")
            "SPC t"  '((lambda () (interactive) (find-file (file-truename (expand-file-name "Notes.org" my:mainorgfolder)))
                    (my:org-narrow-to-header " Tasks")) :which-key "Tasks")
            "SPC n"  '((lambda () (interactive) (find-file (file-truename (expand-file-name "Notes.org" my:mainorgfolder)))
                    (my:org-narrow-to-header " Notes")) :which-key "Notes")
            "SPC b"  '(my:consult-blog :which-key "My Blog Posts")
            )
  :config
  (setq dashboard-projects-backend 'projectile)
  )
```

if you keep changing the looks every week. What you want to achieve is to create patterns (with colors, icons, etc.) that inspire habits over time

the tools to serve these workflows. Now, some people would argue that this is a total waste of time or an unnecessary rabbit hole to customize your tools or workflows. After all you can find some tools that just works out of the box! But  because

Before I show you how I build a dashboard, This  may seem unrelated, since dashboards and themes are independent, in my opinion the key to having a good dashboard is to first chose the right theme. I, personally build workflows in emacs and as a result every piece of the config becomes and integral to a bigger system of "how I do things". The theme, font, colors, etc. are a very useful way of creating patterns and patterns turn into habits over time. A dashboard that does not reinforce such habits would not stick around for long because more efficient workflows and ways of doing things will emerge.


#### main items {#main-items}


#### footer {#footer}


### Making everything more dynamic {#making-everything-more-dynamic}

Now that we covered the basic config of the dashboard, let's take things to the next level and make everything more dynamic. To do this, I am going to implement multiple concepts:

1.  Use Functions to Define Multiple Dashboards:

Working inside emacs, means that we can create funcitons that set the dashboard configuration and bind different keybindings to them. Then we can load any of the dashboards depending on the need. For example you can have `Main` dashboard for project overviews (timeline, priorities, etc.) and a `Dev` dashboard for technical stuff like codes you are working on, and a `Fun` one for ideas you are considering for your next vacation!

1.  Define Contexts:

A great way of customizing Emacs, that allows you to create super-focused work flows is to introduce contexts. For example you can define variable that changes based on time of the day. Then you configure your Emacs to load a dashboard related to work during work hours, and another one for your personal projects in the after hours, or may be you want to separate technical projects (e.g. coding, engineering, ...) from creative projects (making videos, writing blogs,...) or may be you want to define context base don the technology you are using (one for you Mac laptop and another for your Linux server). Having variables in Emacs that track such context is extremely useful for building configs that adapt to your every need.

By combining 1 and 2 above, you can essentially build dynamic dashboards in any imaginable way you wish to.


#### A function to define the main dashboard. {#a-function-to-define-the-main-dashboard-dot}

Let's pull together what we learned in the previous section and put it in one function to load our main dashboard that shows work related projects:

```emacs-lisp
(defun ad/get-motivational-quote ()
(let ((quote (replace-regexp-in-string "\^[.*?m" "" (shell-command-to-string "motivate"))))
(string-match "\"\\(?1:.*\\).*\".*\n.*--\\(?2:.*\\)\n" quote)
(concat "\"" (match-string 1 quote) "\"" "\n\t\t\t\t\t\t'" (match-string 2 quote) "'")))

(defun ad/main-dashboard ()
(interactive)
;;custom faces
 (set-face-attribute 'dashboard-banner-logo-title
                      nil
                      :foreground "#FF0BAF"
                      :weight 'bold)
  (set-face-attribute 'dashboard-text-banner
                      nil
                      :height 1.5
                      :weight 'bold
                      :inherit 'warning)
;;
  (setq dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-footer t
        dashboard-set-init-info t
        dashboard-set-navigator t
        dashboard-item-names '(("Agenda for the coming week:" . "Agenda:"))
        dashboard-week-agenda
        dashboard-banner-logo-title (ad/get-motivational-quote)
        )
```


#### Let's now make a variable to define context and use it to change the dashboard according to the time of the day {#let-s-now-make-a-variable-to-define-context-and-use-it-to-change-the-dashboard-according-to-the-time-of-the-day}
