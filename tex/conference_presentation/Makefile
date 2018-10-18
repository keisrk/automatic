EMACS_CLIENT := emacsclient --alternate-editor=""
define PROJECT_CONF =
((\"$(PROJECT_NAME)\"                                                 \
         :components (\"$(SOURCES)\" \"$(IMAGES)\" \"$(ATTACHMENT)\"))\
 (\"$(SOURCES)\"                                                      \
         :base-directory \"$(BASE_DIR)/\"                             \
         :base-extension \"$(BASE_EXTENSION)\"                        \
         :publishing-directory \"$(PUBLISHING_DIR)/\"                 \
         :publishing-function $(PUBLISHING_FUNCTION)                  \
         :exclude \"$(EXCLUDE)\"                                      \
         :section-numbers nil                                         \
         :body-only t                                                 \
         $(BACKEND_SPECIFICS)                                         \
 )                                                                    \
 (\"$(IMAGES)\"                                                       \
         :base-directory \"$(BASE_DIR)/$(IMAGES)/\"                   \
         :base-extension \"$(IMAGES_EXTENSION)\"                      \
         :publishing-directory \"$(PUBLISHING_DIR)/$(IMAGES)/\"       \
         :publishing-function org-publish-attachment)                 \
 (\"$(ATTACHMENT)\"                                                   \
         :base-directory \"$(BASE_DIR)/\"                             \
         :base-extension \"$(ATTACHMENT_EXTENSION)\"                  \
         :include ($(INCLUDE))                                        \
         :publishing-directory \"$(PUBLISHING_DIR)/\"                 \
         :publishing-function org-publish-attachment)                 \
)
endef
