##
## EPITECH PROJECT, 2023
## B-PDG-300-REN-3-1-PDGD02-guillaume.papineau
## File description:
## Makefile
##

ECHO = /bin/echo -e
DEFAULT = "\033[00m"
BOLD = "\e[1m"
RED = "\e[31m"
GREEN = "\e[32m"
LIGHT_BLUE = "\e[94m"
WHITE = "\e[1;37m"

RM = rm -f

SRC		=   My.hs 	\
			DoOp.hs \

NAME	=	doop

GHC 	= 	ghc

all:
	@$(GHC) $(SRC) -o $(NAME) \
	&& $(ECHO) $(BOLD) $(GREEN)"\n► BUILD SUCCESS !"$(DEFAULT) \
	|| ($(ECHO) $(BOLD) $(RED)"\n► BUILD FAILED"$(DEFAULT) && exit 1)

clean:
	@$(RM) *hi
	@$(RM) *o
	@$(RM) *~
	@$(RM) *#
	@($(ECHO) $(BOLD) $(GREEN)✓$(LIGHT_BLUE)" CLEAN "$(DEFAULT))

fclean:	clean
	@$(RM) $(NAME)
	@$(RM) *.gcda
	@$(RM) *.gcno
	@$(RM) vgcore.*
	@($(ECHO) $(BOLD) $(GREEN)✓$(LIGHT_BLUE)" FCLEAN "$(DEFAULT))

re: fclean all
