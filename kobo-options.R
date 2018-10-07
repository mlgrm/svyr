old_opts <- options(
  koboUser = "mlgrm",
  koboServer = "https://kc.kobotoolbox.org",
  koboToken = "2057910e82d2bd87790f287d69a37ee73f9f7dd6",
  koboID = 200864
)

if (getOption("project")=="wb-energy-bl")
  options(
    koboUser = "samuel_hall",
    koboServer="https://kc.humanitarianresponse.info",
    koboToken="f3702ca2a74efd604a6c5d7fa950b654c0612e97",
    koboID=191842
  )

