% ---------- Class Information ---------- %
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{UCTScienceThesis}

\RequirePackage{expl3} % For LaTeX3 code.

%%% ------------ UCT palette -------------
\RequirePackage[dvipsnames]{xcolor} % Extended color support.
\definecolor{uctblue}{HTML}{009ADA}
\definecolor{uctdark}{HTML}{00243A}
\colorlet{maincolour}{uctblue}
\colorlet{maincolor}{maincolour}



% ---------- Handle Class Options ---------- %
\ExplSyntaxOn
    % --- messages & variables --- %
    \msg_new:nnn { UCTScienceThesis } { color-undefined }
        { Color~'#1'~was~not~defined.~Falling~back~to~black }

    \tl_new:N \g_UCTScienceThesis_myschool_tl
    \tl_new:N \g_UCTScienceThesis_mycover_tl
    \tl_new:N \g_UCTScienceThesis_mylanguage_tl
    \tl_new:N \g_UCTScienceThesis_style_tl
    \tl_new:N \g_UCTScienceThesis_docstage_tl
    \tl_new:N \g_UCTScienceThesis_media_tl
    \tl_new:N \g_UCTScienceThesis_linkcolor_tl
    \tl_new:N \g_UCTScienceThesis_doctype_tl

    \tl_gset:Nn \g_UCTScienceThesis_myschool_tl   {uctscience}
    \tl_gset:Nn \g_UCTScienceThesis_mycover_tl    {classic}
    \tl_gset:Nn \g_UCTScienceThesis_mylanguage_tl {english}
    \tl_gset:Nn \g_UCTScienceThesis_style_tl      {classic}
    \tl_gset:Nn \g_UCTScienceThesis_docstage_tl   {working}
    \tl_gset:Nn \g_UCTScienceThesis_media_tl      {paper}
    \tl_gset:Nn \g_UCTScienceThesis_linkcolor_tl  {black}
    \tl_gset:Nn \g_UCTScienceThesis_doctype_tl    {thesis}

    % --- helper --- %
    \cs_new:Npn \UCTScienceThesis_check_color:n #1
    {
        \color_if_exist:nTF {#1}
        { \tl_set:Nn \l_tmpa_tl {#1} } % whatever your “on-true” code is
        {
            \tl_set:Nn \l_tmpa_tl {black} % fallback
            \msg_warning:nnn {UCTScienceThesis}{color-undefined}{#1}
        }
    }
    % --- key-value interface --- %
    \keys_define:nn { UCTScienceThesis } {
        % school -----------------------------------------------------------
        school .choices:nn =
            { uctscience }
            { \tl_gset:Nn \g_UCTScienceThesis_myschool_tl { #1 } },
        school / unknown .code:n = 
            { \ClassWarningNoLine{UCTScienceThesis}
            {Specified~'school'~not~available.~Defaulting~to~'uctscience'} },
        school .default:n = { uctscience },
        school .initial:n = { uctscience },

        % cover -----------------------------------------------------------
        coverstyle .choices:nn =
            { classic, bw }
            { \tl_gset:Nn \g_UCTScienceThesis_mycover_tl { #1 } },
        coverstyle / unknown .code:n = 
            { \ClassWarningNoLine{UCTScienceThesis}{Specified~'coverstyle'~not~available.~Defaulting~to~'classic'} },
        coverstyle .default:n = { classic },
        coverstyle .initial:n = { classic },

        % language -----------------------------------------------------------
        language .choices:nn = 
            { english } 
            { \tl_gset:Nn \g_UCTScienceThesis_mylanguage_tl { #1 } },
        language / unknown .code:n = 
            { \ClassWarningNoLine{UCTScienceThesis}{Specified~'language'~not~available.~Defaulting~to~'english'} },
        language .default:n = { english },
        language .initial:n = { english },

        % chapter -----------------------------------------------------------
        chapterstyle .choices:nn = 
            { classic, fancy, modern } 
            { \tl_gset:Nn \g_UCTScienceThesis_style_tl { #1 } },
        chapterstyle / unknown .code:n = 
            { \ClassWarningNoLine{UCTScienceThesis}{Specified~'chapterstyle'~not~available.~Defaulting~to~'classic'} },
        chapterstyle .default:n = { classic },
        chapterstyle .initial:n = { classic },

        % docstage -----------------------------------------------------------
        docstage .choices:nn = 
            { working, final } 
            { \tl_gset:Nn \g_UCTScienceThesis_docstage_tl { #1 } },
        docstage / unknown .code:n = 
            { \ClassWarningNoLine{UCTScienceThesis}{Specified~'docstage'~not~available.~Defaulting~to~'working'} },
        docstage .default:n = { working },
        docstage .initial:n = { working },

        % media -----------------------------------------------------------
        media .choices:nn = 
            { paper, screen } 
            { \tl_gset:Nn \g_UCTScienceThesis_media_tl { #1 } },
        media / unknown .code:n = 
            { \ClassWarningNoLine{UCTScienceThesis}{Specified~'media'~not~available.~Defaulting~to~'paper'} },
        media .default:n = { paper },
        media .initial:n = { paper },

        % link colour -----------------------------------------------------------
        linkcolor .code:n = 
            {
                \UCTScienceThesis_check_color:n {#1}      % sets \l_tmpa_tl
                \tl_gset_eq:NN \g_UCTScienceThesis_linkcolor_tl \l_tmpa_tl        
            },
        linkcolor .default:n = { black },
        linkcolor .initial:n = { uctblue },


        % document type -----------------------------------------------------------
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

\ExplSyntaxOff

\ProcessKeyOptions[UCTScienceThesis]

\ExplSyntaxOn
\cs_new_nopar:Npx \LanguageOption { \tl_use:N \g_UCTScienceThesis_mylanguage_tl }
\cs_new_nopar:Npx \SchoolOption   { \tl_use:N \g_UCTScienceThesis_myschool_tl }
\cs_new_nopar:Npx \CoverOption    { \tl_use:N \g_UCTScienceThesis_mycover_tl }
\cs_new_nopar:Npx \ChapterOption  { \tl_use:N \g_UCTScienceThesis_style_tl }
\cs_new_nopar:Npx \DocStageOption { \tl_use:N \g_UCTScienceThesis_docstage_tl }
\cs_new_nopar:Npx \MediaOption    { \tl_use:N \g_UCTScienceThesis_media_tl }
\cs_new_nopar:Npx \ColorOption    { \tl_use:N \g_UCTScienceThesis_linkcolor_tl }
\cs_new_nopar:Npx \DocTypeOption  { \tl_use:N \g_UCTScienceThesis_doctype_tl }
\ExplSyntaxOff


%%% Load Master Class %%%
\LoadClass[a4paper,twoside,11pt]{report}

% ---------- Load Babel Package (Language) %%%
\expandafter\RequirePackage\expandafter[\LanguageOption]{babel}

% ---------- Required Packages %%%
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


% Geometry defaults
\RequirePackage{geometry}
\geometry{a4paper,inner=2.5cm,outer=2cm,top=2.2cm,bottom=2.2cm}

% --- Global line spacing ---
\AtBeginDocument{\onehalfspacing}

% --- Hyperref colour defaults - honour user override later ---
\AtBeginDocument{
    \hypersetup{
        linkcolor = \ColorOption,
        citecolor = uctdark,
        urlcolor  = uctblue,
        pdfborder = 0 0 0
    }
}

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

% ---------- Import Configurations ---------- %
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

% ---------- User-facing helpers ---------- %
\newcommand{\UCTFrontMatter}{
    \pagenumbering{roman}
    \setcounter{page}{1}
}

\newcommand{\UCTMainMatter}{
    \cleardoublepage
    \pagenumbering{arabic}
}