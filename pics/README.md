# `pics`

`beautier` pictures.

## How did you convert the fuzzy white background to one single color?

```
convert butterfly.png -fuzz 15% -fill white -opaque white butterfly_mono_background.png
convert butterfly_mono_background.png -background white -alpha remove butterfly_mono_background_2.png
```
