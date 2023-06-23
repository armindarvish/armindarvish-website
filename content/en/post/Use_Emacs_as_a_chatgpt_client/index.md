---
title: "Use Emacs as a ChatGPT App from Anywhere on Your System"
author: ["Armin Darvish"]
date: 2023-06-22T18:44:00-07:00
lastmod: 2023-06-22T18:44:27-07:00
draft: false
weight: 3005
summary: "In this post, I will show you how you can quickly make a chatgpt/bing client inside emacs that you can call from anywhere on your system (e.g. spotlight search on macOS)"
authors:
  - admin
projects: [software]
categories: [software, ChatGPT, Emacs]
featured: false
commentable: true
image:
  caption:
  focal_point: 'center'
---

In this post, I will show you how you can easily create a ChatGPT app on your system using the existing packages for ChatGPT in Emacs and call it from anywhere on your system. If you are on a system where you don't have an app (like macOS or Linux) and don't like to using the browser, this might be a good way of using ChatGPT plus the fact that you can integrate this with all sorts of other things you can do inside Emacs.


## ChatGPT Clients in Emacs {#chatgpt-clients-in-emacs}

There are a number of packages for ChatGPT client inside Emacs and I have tested a few of them so far. In my opinion, Karthik Chikmagalur's [gptel](https://github.com/karthink/gptel) is the best one both in terms of functionality and ease-of-use and also in terms of implementation and staying true to Emacs way of doing things. Therefore, if you are not interested in testing things out, just go ahead and use gptel and you can skip the rest of this section. There is a good (but long) live stream by David Wilson on System Crafters channel if you want to see gptel in action: [Crafting the Future: AI Assistants in Emacs - System Crafters Live! - YouTube](https://www.youtube.com/watch?v=JImYEdqVQR8).

If you want to know more about different packages, here are some notes:


### GPTel {#gptel}

As I said above [gptel](https://github.com/karthink/gptel) is the best client I have tested so far. It is simple, does not have weird dependencies or authentication process, and provides an emacs-like experience (e.g. works in any buffer, does not create a million different buffers, ...) It is fast and async, and installation is straightforward since it is on MELPA. The code itself is also clean and easy to read, if you want to tinker with it and create your own custom functions or macros.


### emacs-aichat {#emacs-aichat}

[emacs-aichat](https://github.com/xhcoding/emacs-aichat) works as expected but is not as clean in implementation as gptel. It does offer the option to use Microsoft's BingAI instead of ChatGPT but otherwise is not as good as gptel. It uses [emacs-websocket](https://github.com/ahyatt/emacs-websocket) and [emacs-async-await](https://github.com/chuntaro/emacs-async-await) which adds extra dependencies and the way the code is implemented there are many layers of functions and macros calling each other even though at the end it is using `url-asynchronous` to make calls to APIs. There are duplicates of similar functionalities for BingAI v.s. OpenAI's ChatGPT and different ways to do authentication or setup for them. As a result the code is complicated to read and debugging (which is expected with experimental packages like this) or building on top of it is not as easy. That said the author is quite responsive and has been improving things frequently.


### openai and chatgpt {#openai-and-chatgpt}

You can take a look at [openai](https://github.com/emacs-openai/openai) and [chatgpt](https://github.com/emacs-openai/chatgpt), and other related packages by Jen-Chieh Shen.  [openai](https://github.com/emacs-openai/openai) provides low-level calls to OpenAI API and [chatgpt](https://github.com/emacs-openai/chatgpt) builds on top of that for using the ChatGPT. The author has also other packages for other OpenAI services. I think the implementation is very neat and everything is done in a modular way, so this is a good reference if you want to learn and perhaps even build your own ChatGPT client inside Emacs. That said, I personally don't like the interface of the [chatgpt](https://github.com/emacs-openai/chatgpt) package. It opens up new buffers for inserting queries and does not play well with evil-mode. The gimmicky UI elements like spinners and so on seem unnecessary. They do not provide extra functionalities and are not pretty enough to add value to the user experience either. I think if you spend enough time configuring the package and trimming things up, this can be a decent ChatGPT client, perhaps with more features than some other packages I've tried, but I prefer a simpler interface that are faster and more user-friendly.


### org-ai {#org-ai}

[org-ai](https://github.com/rksm/org-ai) takes a different approach to interacting with ChatGPT (and in this case also DALL-E) and that is by using source blocks inside Emacs org-mode. I tried it shortly and decided that I personally prefer other approaches. The reason is inserting ChatGPT responses inside an org source block means that the code blocks in ChatGPT responses are now nested inside another code block which is not useful. I probably would have preferred if the response was returned as result (similar to [ob-chatgpt](https://github.com/suonlight/ob-chatgpt) below). Also, I don't think a conversation with AI using natural language really belongs in a source block (which is really designed for coding). But that's just my personal opinion. I can see how some people may prefer this over other approaches. You can try it yourself and see if it fits your use case.


### ChatGPT.el and ob-chatgpt {#chatgpt-dot-el-and-ob-chatgpt}

Josh Cho's [ChatGPT.el](https://github.com/joshcho/ChatGPT.el) is perhaps the worst approach among the ones I have tried. The implementation is unnecessarily complicated. It uses python and [chatgpt-wrapper](https://github.com/mmabrouk/chatgpt-wrapper) under the hood to interface with OpenAI API and all it really does is to run a `shell-command` that does `pip install chatgpt-wrapper`. The authentication process uses an external browser and is very clunky and often requires re-authentication because it is using chatgpt-wrapper under the hood which runs python scripts. In addition, there is Minh Nguyen-Hue's [ob-chatgpt](https://github.com/suonlight/ob-chatgpt) which is built on ChatGPT.el but uses org-babel source blocks instead. Since it is using ChatGPT.el, it has all the issues of ChatGPT.el as well.


## Configuring GPTel {#configuring-gptel}

Install [gptel](https://github.com/karthink/gptel) following the official documents, then all you have to do is to set up your API key by setting `gptel-api-key` and you are good to go. You can call the interactive command gptel by `M-x gptel` and you will jump to a ChatGPT buffer where you can type your prompts and submit them by `C-c RET`. If you have access to **GPT 4.0**, you can set that up by setting `gptel-model`. You can also use `org-mode` instead of `markdown-mode` by setting `gptel-default-mode` but **be aware that this converts the response to org-mode by simple regex replacement and often creates mistakes and bugs**. For example, I have noticed random **=** signs in elisp codes because the conversion replaces **\`** with **=** to convert inline code from markdown to org-mode, and sometimes it makes a mistake and does that inside a code block.

Here is an example code for installing gptel using `use-package`:

```emacs-lisp
(use-package gptel
   :config
   (setq gptel-api-key (auth-source-pick-first-password :host "openai.com"))
   (setq gptel-default-mode 'org-mode)
   (setq gptel-model "gpt-3.5-turbo")
   )
```

Note that I am using auth-source to store my OpenAI's API key so I don't need to put it in my config. Refer to [gptel documents](https://github.com/karthink/gptel#usage) for more info on authentication.


## Creating a Custom Function to Call gptel {#creating-a-custom-function-to-call-gptel}

Now that we have gptel installed and running, you can go ahead and create an interactive command to call gptel. We can then use this command to call `gptel` from outside Emacs with emacsclient. Here is a sample function that creates a new frame and runs gptel.

```emacs-lisp
(defun ad/ai-from-anywhere ()
(interactive)
(let* ((screen-width (display-pixel-width))
       (screen-height (display-pixel-height))
       (frame-width (/ screen-width 3))
       (frame-height screen-height)
       (frame-left (- screen-width frame-width))
       (frame-top 0)
       (chat-frame (make-frame `((window-system . ns)  ;;change this if you are not on macOS. For example you can use "x" instead of "ns" for x systems. Refer to make-frame documentation for more details
                            (top . ,frame-top)
                            (left . ,frame-left)
                            (width . (text-pixels . ,frame-width))
                            (heigth . (text-pixels . ,frame-height))
                            (minibuffer . t)
                            ))))
  (select-frame chat-frame)
  )
  (add-hook 'gptel-post-response-hook (lambda () (goto-char (point-max))))
  (gptel "My:AI Chat" gptel-api-key nil)
  (switch-to-buffer "My:AI Chat")
  (delete-other-windows)
)
```

Note how I set the window-system to `ns` (this is for macOS or Linux) and set the frame size and position in pixels by changing `width`, `height`, `top`, `left`, etc. when calling `make-frame`. Refer to Emacs help on `make-frame` or the manual on [Frame Parameters](https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Parameters.html#Frame-Parameters).


## Setup your Emacs Server {#setup-your-emacs-server}

From inside Emacs, you CNA simply run `server-start` to start an Emacs server, and then you can run `emacsclient` from terminal. If you want to run the server as **daemon** in the background, look at [EmacsWiki: Emacs As Daemon](https://www.emacswiki.org/emacs/EmacsAsDaemon). If you are on macOS, this [link](https://briansunter.com/blog/emacs-daemon-macos) can provide some more details in addition to EmacsWiki article.


## Create a script to call Emacsclient {#create-a-script-to-call-emacsclient}

Once you have the server set up and running, you can call emacsclient and run your interactive command to create a new frame and start gptel.


### macOS {#macos}

On macOS, you can do this by writing an apple script and call it from spotlight search similar to this article for org-capture: [Org capture from everywhere in macOS | macOS &amp; (open-source) Software](https://macowners.club/posts/org-capture-from-everywhere-macos/). Simply, open **Script Editor** and make a new **Application** with content similar to the following code. Make sure you adjust the path to your emacsclient and also the name of the interactive command you are calling. Note how I use `select-frame-set-input-focus` to make sure the window is focused.

```apples
on run
    try
        do shell script "/opt/homebrew/bin/emacsclient -e \"(progn (select-frame-set-input-focus (selected-frame)) (ad/ai-from-anywhere))\"> /dev/null 2>&1 &"
    if application "Emacs" is running then
        tell application "Emacs" to activate
    end if
    end try
end run
```

Save the file as **Application** in your default applications folder and name it something easy to remember like `myai`.

Then call the spotlight search (bound to `⌘ SPC` by default) and search for your new application (e.g. `myai`). When you run it, it should create a new Emacs frame, and you can start chatting with GPT right away.


### KDE Plasma {#kde-plasma}

You can do something similar in KDE Plasma. You can create a custom shell script with the following content:

```shell
#!/bin/bash
emacsclient -e \"(progn (select-frame-set-input-focus (selected-frame)) (ad/ai-from-anywhere))\"
```

Save the script in a convenient location such as `~/.local/bin/myai` and make sure to make it executable by using `chmod u+x ~/.local/bin/myai` . Then call the `KDERunner` (bound to `Alt SPC` by default) and search for your shell script.


### Other Systems {#other-systems}

I have not tried this on other systems, but you can similarly make shell scripts to call emacsclient and run similar searches on most systems (windows button on Windows, unity search on, etc.) and you will have a ChatGPT app.


## Screenshot {#screenshot}

That's it. Now go get a cup of coffee and start a conversation with our future overlords. Make sure you are nice to them!

Here is a screen shot of my ChatGPT app on macOS:

{{< figure src="/ox-hugo/Screenshot.gif" width="800px" height="nilpx" >}}
