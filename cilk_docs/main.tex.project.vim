" ATP project vim script: Tue Apr 16, 2013 at 01:48 PM -0400.

let b:atp_MainFile = 'main.tex'
let g:atp_mapNn = 0
let b:atp_autex = 1
let b:atp_TexCompiler = 'pdflatex'
let b:atp_TexOptions = '-synctex=1'
let b:atp_TexFlavor = 'tex'
let b:atp_auruns = '1'
let b:atp_ReloadOnError = '1'
let b:atp_OpenViewer = '1'
let b:atp_XpdfServer = 'main'
let b:atp_Viewer = 'okular'
let b:TreeOfFiles = {'scheduler.tex': [{}, 69], 'intro.tex': [{}, 67], 'cilk-abi.tex': [{}, 70], 'cilk_api.tex': [{}, 68]}
let b:ListOfFiles = ['macro-defs.tex', 'editingmarks.tex', 'intro.tex', 'cilk_api.tex', 'scheduler.tex', 'cilk-abi.tex', 'refs.bib']
let b:TypeDict = {'macro-defs.tex': 'preambule', 'scheduler.tex': 'input', 'cilk-abi.tex': 'input', 'intro.tex': 'input', 'refs.bib': 'bib', 'editingmarks.tex': 'preambule', 'cilk_api.tex': 'input'}
let b:LevelDict = {'macro-defs.tex': 1, 'scheduler.tex': 1, 'cilk-abi.tex': 1, 'intro.tex': 1, 'refs.bib': 1, 'editingmarks.tex': 1, 'cilk_api.tex': 1}
let b:atp_BibCompiler = 'bibtex'
let b:atp_StarEnvDefault = ''
let b:atp_StarMathEnvDefault = ''
let b:atp_updatetime_insert = 4000
let b:atp_updatetime_normal = 2000
