nces_pals <- list(
  "seq" = list(
    "blues" = c("#00378C", "#114EA4", "#2666B9", "#3D7FCC", "#5698DC", "#70B2E9", "#8ECBF4", "#B0E5FA", "#EAF9FD"),
    "reds" = c("#971B2F", "#AE3530", "#C44E31", "#D86735", "#E8813E", "#F59D51", "#FDB970", "#FFD69E", "#FAF3D9"),
    "greens" = c("#008624", "#3C9426", "#5CA22A", "#79AF33", "#93BD41", "#AECA55", "#C7D872", "#DEE59C", "#EFF1DF"),
    "purples" = c("#84329B", "#A43F9F", "#BC53A7", "#CF69B0", "#DF82BA", "#EC9BC6", "#F4B5D3", "#F8D1E0", "#F5EEEF"),
    "blgryl" = c("#071D49", "#11375B", "#1B526A", "#266F73", "#338E73", "#58AC60", "#9AC362", "#CDD984", "#FCF0B7"),
    "puoryl" = c("#84329B", "#A93D97", "#C94E88", "#E56270", "#FB7A4E", "#FD9B4F", "#FFB965", "#FFD589", "#FCF0B7")
  ),
  "div" = list(
    "rdbl" = c("#971B2F", "#BB4330", "#DA6B36", "#F2954D", "#FDC184", "#F5EFE4", "#86D4F8", "#5EAAE9", "#3A82D0", "#1A5CB1", "#00378C"),
    "grpu" = c("#008624", "#509C28", "#7EB236", "#A9C752", "#D2DC84", "#F2EFEA", "#ED83BB", "#D96AB0", "#C254A7", "#A7409F", "#84329B"),
    "rdorgrbl" = c("#971B2F", "#BF402B", "#E66428", "#F99431", "#FBC772", "#FAF3D9", "#5EA46C", "#2E8271", "#205E6C", "#133D5D", "#071D49")
  )
)

library(usethis)
usethis::use_data(nces_pals, internal=TRUE, overwrite=TRUE)
