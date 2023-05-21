---
title: "testing my ai assistant"
author: ["Armin Darvish"]
lastmod: 2023-05-19T10:32:36-07:00
draft: true
weight: 3005
authors:
  - admin
projects: []
featured: false
commentable: true
image:
  caption: 'caption'
  focal_point: 'center'
---

## Question {#question}

how do I do a rolling window cross-correlation to find common events in two time-series in python?


## ChatGPT {#chatgpt}

You can use the `numpy.correlate()` function to perform cross-correlation between the two time series. To implement a rolling window, you can slice the two time series into overlapping segments, then perform the correlation on each segment. Here's an example code:

```python
import numpy as np
from scipy import signal as sig

def rolling_corr(x, y, window):
    # Calculate the length of the output array
    n = len(x) - window + 1
    # Initialize the output array
    out = np.empty(n)
    # Loop through each rolling window and compute the correlation
    for i in range(n):
        x_win = x[i:i+window]
        y_win = y[i:i+window]
        # out[i] = np.correlate(x_win, y_win, mode='valid')[0]
        out[i] = np.corrcoef(x_win, y_win)[0,1]
    # Return the output array
    return out
```

In this function, `x` and `y` are the two time series, and window is the size of the rolling `window`. The function returns an array of length `n`, where `n` is the number of rolling `windows`. Each element of the array represents the correlation coefficient between the two time series for a given window.

To find common events, you can define a threshold for the correlation coefficient and identify the segments where the coefficient exceeds the threshold.


## Question {#question}

Can you write some code to plot the time series in the previous code for me to visually see the effect
Assistant:


## ChatGPt {#chatgpt}

Sure! Here's some code that generates two example time series and plots them along with the cross-correlation:

```python
import matplotlib.pyplot as plt
import mplcyberpunk

# plt.style.use("mywebsite-dark")
plt.style.use("mycyberpunk")

# Generate example time series
t = np.arange(0, 10, 0.1)
x = np.sin(t*2) + np.random.normal(scale=0.1, size=len(t))
# y = np.sin(t*1.1 + 0.5) + np.random.normal(scale=0.1, size=len(t))
y = np.sin(t*4-np.pi/2) + np.random.normal(scale=0.1, size=len(t))
# y = -x
x_n= (x - np.mean(x))/(np.std(x))
y_n= (y - np.mean(y))/(np.std(y))
# Calculate rolling cross-correlation with window size 50
corr = rolling_corr(x_n, y_n, 50)

sel = np.where(corr<-0.1)
# Plot time series and cross-correlation
fig, ax = plt.subplots(ncols=2, figsize=(12,4))
ax[0].plot(t, x, label='x')
ax[0].plot(t, y, label='y')
ax[0].set_xlabel('Time')
ax[0].set_ylabel('Value')
ax[0].legend()

ax[1].plot(t[:len(corr)], corr)
ax[1].plot(t[sel], corr[sel], "ro")
ax[1].set_xlabel('Time lag')
ax[1].set_ylabel('Cross-correlation')
ax[1].axhline(0.5, color='red', linestyle='--')
ax[1].axhline(-0.5, color='red', linestyle='--')

plt.show()
```

{{< figure src="featured.png" >}}

Here, we generate two time series with a similar waveform but slightly different frequencies, and add some random noise to each of them. We then calculate the rolling cross-correlation using a window size of 50, and plot the two time series and the cross-correlation side-by-side. The dashed red lines show the threshold for the correlation coefficient.

You can modify the example code to use your own time series data and parameters.
