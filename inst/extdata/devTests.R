library(rpackageutils)


rpackageutils::download_unpack_zip(
  #url="https://zenodo.org/record/4404738/files/mengqi-z/demeter-v1.1.0-wild2020-ArgentinaNexus.zip?download=1",
  url="https://zenodo.org/record/4641500/files/IDBNexus_gcam5p3_HadGEM2-ES_rcp8p5.zip?download=1",
  data_directory = getwd()) -> datadir

dir.exists(datadir)
