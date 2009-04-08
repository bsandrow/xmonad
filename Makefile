PREFIX = $(HOME)
BINDIR = $(PREFIX)/scripts

conf_target_dir 	= $(PREFIX)/.xmonad
script_target_dir 	= $(BINDIR)
target_dirs			= $(script_target_dir) $(conf_target_dir)

conf_src_dir 	= .
script_src_dir 	= scripts
src_dirs		= $(script_src_dir) $(conf_src_dir)

script_targets = $(addprefix $(script_target_dir)/, $(scripts))
script_srcs    = $(addprefix $(script_src_dir)/,    $(scripts))
conf_targets   = $(addprefix $(conf_target_dir)/, $(confs))
conf_srcs      = $(addprefix $(conf_src_dir)/,    $(confs))

confs = \
	xmonad.hs

scripts = \
	xmonad-bottom-bar.sh\
	xmonad-startup.sh


install: $(target_dirs) $(script_targets) $(conf_targets)

$(target_dirs):
	mkdir -p $@

$(conf_targets): $(conf_srcs)
	cp $? $(conf_target_dir)/

$(script_targets): $(script_srcs)
	cp $? $(script_target_dir)/
