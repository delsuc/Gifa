iter 200 ndisp 20
algo 11
buttonbox DOSY \
        "Tab_buffer" tab_buffer \
        "Showtab" showtab \
        separator \
        'Eval noise' 'evaln.g' \
	'Calibdosy' calibdosy \
        'Dosy_setup' dosy_setup \
        separator \
        "Dosyfit (1 comp.)" dosyfit \
        "Dosyfit_2 (2 comp.)" dosyfit_2 \
        "Inv Laplace" invlap \
        "Continue" "iter $_ invlapcont" \
        "Inv tab Laplace" invtlap \
        "Continue" "iter $_ invtlapcont" \
        separator \
        'Dosy2D' dosy2d.g \
        'Dosy3D' dosy3d.g \
        *
