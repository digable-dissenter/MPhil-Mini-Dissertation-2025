%%% Class Information %%%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{UCTScienceThesis}[2025/05/29 UCT Science Thesis Class]

\newif\ifUCT@printglossary
\UCT@printglossaryfalse

\RequirePackage{expl3} % Required for writing LaTeX3 code in this class.
\cs_new:Npn \UCTScienceThesis_check_color:n {
    \color_if_exist:NTF #1
        { }
        {
            \tl_gset:Nn \g_UCTScienceThesis_linkcolor_tl { black }
            \ClassWarningNoLine{UCTScienceThesis}
                {Specified~'linkcolor'~'#1'~undefined;~defaulting~to~black}
        }
}

%%% Handle Class Options %%%
\ExplSyntaxOn

\keys_define:nn { UCTScienceThesis } {
    school .choices:nn =
        { uctscience }
        { \tl_gset:Nn \g_UCTScienceThesis_myschool_tl { #1 } },
    school / unknown .code:n = 
        { \ClassWarningNoLine{UCTScienceThesis}
        {Specified~'school'~not~available.~Defaulting~to~'uctscience'} },
    school .default:n = { uctscience },
    school .initial:n = { uctscience },

    coverstyle .choices:nn =
        { classic, bw }
        { \tl_gset:Nn \g_UCTScienceThesis_mycover_tl { #1 } },
    coverstyle / unknown .code:n = 
        { \ClassWarningNoLine{UCTScienceThesis}{Specified~'coverstyle'~not~available.~Defaulting~to~'classic'} },
    coverstyle .default:n = { classic },
    coverstyle .initial:n = { classic },

    language .choices:nn = 
        { english } 
        { \tl_gset:Nn \g_UCTScienceThesis_mylanguage_tl { #1 } },
    language / unknown .code:n = 
        { \ClassWarningNoLine{UCTScienceThesis}{Specified~'language'~not~available.~Defaulting~to~'english'} },
    language .default:n = { english },
    language .initial:n = { english },

    chapterstyle .choices:nn = 
        { classic, fancy, modern } 
        { \tl_gset:Nn \g_UCTScienceThesis_style_tl { #1 } },
    chapterstyle / unknown .code:n = 
        { \ClassWarningNoLine{UCTScienceThesis}{Specified~'chapterstyle'~not~available.~Defaulting~to~'classic'} },
    chapterstyle .default:n = { classic },
    chapterstyle .initial:n = { classic },

    docstage .choices:nn = 
        { working, final } 
        { \tl_gset:Nn \g_UCTScienceThesis_docstage_tl { #1 } },
    docstage / unknown .code:n = 
        { \ClassWarningNoLine{UCTScienceThesis}{Specified~'docstage'~not~available.~Defaulting~to~'working'} },
    docstage .default:n = { working },
    docstage .initial:n = { working },

    media .choices:nn = 
        { paper, screen } 
        { \tl_gset:Nn \g_UCTScienceThesis_media_tl { #1 } },
    media / unknown .code:n = 
        { \ClassWarningNoLine{UCTScienceThesis}{Specified~'media'~not~available.~Defaulting~to~'paper'} },
    media .default:n = { paper },
    media .initial:n = { paper },

    linkcolor .code:n = {
        \tl_gset:Nn \g_UCTScienceThesis_linkcolor_tl { #1 }
        \UCTScienceThesis_check_color:n { #1 }
        \tl_if_eq:NNTF \l_tmpa_tl \q_no_value {
            \tl_gset:Nn \g_UCTScienceThesis_linkcolor_tl { black }
            \ClassWarningNoLine{UCTScienceThesis}{Specified~'linkcolor'~not~available.~Defaulting~to~'black'}
        }
    },
    linkcolor .default:n = { black },
    linkcolor .initial:n = { black },

    doctype .choices:nn = 
        { thesis, report } 
        { \tl_gset:Nn \g_UCTScienceThesis_doctype_tl { #1 } },
    doctype / unknown .code:n = 
        { \ClassWarningNoLine{UCTScienceThesis}{Specified~'doctype'~not~available.~Defaulting~to~'thesis'} },
    doctype .default:n = { thesis },
    doctype .initial:n = { thesis },
    
    unknown .code:n = 
        { \ClassWarningNoLine{UCTScienceThesis}{An~unknown~option~was~ignored.~Please~check~the~documentation~to~see~the~available~options} }
}

\newcommand{\LanguageOption}{\g_UCTScienceThesis_mylanguage_tl}
\newcommand{\SchoolOption}{\g_UCTScienceThesis_myschool_tl}
\newcommand{\CoverOption}{\g_UCTScienceThesis_mycover_tl}
\newcommand{\ChapterOption}{\g_UCTScienceThesis_style_tl}
\newcommand{\DocStageOption}{\g_UCTScienceThesis_docstage_tl}
\newcommand{\MediaOption}{\g_UCTScienceThesis_media_tl}
\newcommand{\ColorOption}{\g_UCTScienceThesis_linkcolor_tl}
% \newcommand{\DocTypeOption}{g_UCTScienceThesis_doctype_tl}

\ProcessKeyOptions[UCTScienceThesis]
\ExplSyntaxOff

%%% Load Master Class %%%
\LoadClass[a4paper,twoside,11pt]{report}

%%% Load Babel Package (Language) %%%
\expandafter\RequirePackage\expandafter[\LanguageOption]{babel}

%%% Required Packages %%%
\RequirePackage{ifthen} % Provides conditional statements.
\RequirePackage{etoolbox} % Advanced programming tools for LaTeX.
\RequirePackage{graphicx} % For including and manipulating images.
\RequirePackage{subcaption} % Subfigures and side-by-side captions.
\RequirePackage{tabularx} % Flexible-width tables.
\RequirePackage{xltabular} % Same as tabularx but to be used in long tables.
\RequirePackage{booktabs} % Professional table styling.
\RequirePackage{multirow} % Support for multirow cells in tables.
\RequirePackage{longtable} % Tables spanning multiple pages.
\RequirePackage{caption} % Customisation of captions.
\RequirePackage{enumitem} % Customisation of lists.
\RequirePackage{amsmath} % Advanced mathematical expressions.
\RequirePackage{amssymb} % Extended mathematical symbols.
\RequirePackage[dvipsnames]{xcolor} % Extended color support.
%%% ------------ UCT palette & geometry defaults -------------
\definecolor{uctblue}{HTML}{009ADA}
\definecolor{uctdark}{HTML}{00243A}
\colorlet{maincolour}{uctblue}
\colorlet{maincolor}{maincolour}

\RequirePackage{geometry}
\geometry{a4paper,inner=2.5cm,outer=2cm,top=2.2cm,bottom=2.2cm}

% --- Global line spacing ---
\RequirePackage{setspace}
\AtBeginDocument{\onehalfspacing}

\providecommand{\DocumentVersion}[1]{}

% --- Hyperref colour defaults - honour user override later ---
\AtBeginDocument{
    \hypersetup{
        linkcolor = \g_UCTScienceThesis_linkcolor_tl,
        citecolor = uctdark,
        urlcolor  = uctblue,
        pdfborder = 0 0 0
    }
}

\newcommand{\DocTypeOption}{\g_UCTScienceThesis_doctype_tl}
%%% -----------------------------------------------------------
\RequirePackage{tcolorbox} % Creating colored and highlighted boxes.
\RequirePackage{varwidth} % Allows variable-width boxes.
\RequirePackage{titlesec} % Customisation of section and chapter titles.
\RequirePackage{titling} % Control over title and titlepage formatting.
\RequirePackage[toc,page]{appendix} % Formatting for appendices.
\RequirePackage[acronym]{glossaries} % Create glossaries and acronyms.
\RequirePackage[calc,datesep={/}]{datetime2} % Enhanced date and time formatting.
\RequirePackage[bookmarks,pdfusetitle]{hyperref} % Hyperlinks and PDF metadata.
\RequirePackage{xparse} % Advanced argument parsing for custom macros.
\RequirePackage{typearea} % Page layout adjustments (KOMA-Script).
\RequirePackage{eso-pic} % Adding elements to the page background.
\RequirePackage{setspace} % Adjust line spacing.
\RequirePackage[newfloat]{minted} % Syntax highlighting for code.
\RequirePackage{silence} % Suppress specific package warnings.
\RequirePackage{fontawesome5} % Font awesome icons.
\RequirePackage{calc} % Infix notation arithmetic.
\RequirePackage{contour} % Countour words in a given color.
\RequirePackage[normalem]{ulem} % A better underline.
\RequirePackage{mfirstuc} % Capitalise words in a sentence.
\RequirePackage{tikz} % Create high-quality graphics programmatically.
\RequirePackage{psvectorian} % To include in-text ornamatentions. 
\RequirePackage{blindtext} % Generate placeholder text.

\tcbuselibrary{most} % Load most libraries from the tcolorbox package.
\WarningsOff[blindtext] % Suppress warnings from the blindtext package.

%%% Import Configurations - Maintain the Specified Order! %%%
\usepackage{style-config/00_fonts}
\usepackage{style-config/01_colours}
\usepackage{style-config/02_margins}
\usepackage{style-config/03_references}
\usepackage{style-config/04_headers}
\usepackage{style-config/05_contents}
\usepackage{style-config/06_glossary}
\usepackage{style-config/07_chapters}
\usepackage{style-config/08_tables}
\usepackage{style-config/09_code}
\usepackage{style-config/10_macros}
\usepackage{style-config/11_metadata}

% ---------- User-facing helpers ----------
\newcommand{\UCTFrontMatter}{
    \pagenumbering{roman}
    \setcounter{page}{1}
}

\newcommand{\UCTMainMatter}{
    \cleardoublepage
    \pagenumbering{arabic}
}