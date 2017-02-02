#!/bin/sh

# ./_build.sh       -> rmarkdown::render_site()
# ./_build.sh clean -> rmarkdown::clean_site(preview = TRUE)

case "$1" in
        clean )
            echo "Arquivos que serão apagados:"
            Rscript -e "rmarkdown::clean_site(preview = TRUE)"
            echo "Deseja limpar os arquivos? [y/n]"
            read opcao;  echo $opcao
            if [ $opcao = "y" ];
            then
              echo "Renderizando site."
              Rscript -e "rmarkdown::clean_site()"
              echo "\n"
            fi
            ;;
        * )
            Rscript -e "rmarkdown::render_site()"
            ;;
    esac
