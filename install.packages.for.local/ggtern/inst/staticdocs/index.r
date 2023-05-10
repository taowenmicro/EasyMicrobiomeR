sd_section("Starting Points",
           "Introductory material",
           c("ggtern_package",
             "ggtern",
             "ggplot"))

sd_section("Ternary Coordinates",
           "Definitions for the ternary coordinate system.",
           c("coord_tern")
)

sd_section("Ternary Scales",
           "Definitions for the ternary axes.",
           c("scale_X_continuous",
             'tern_limits')
)

sd_section("Approved Layers",
           "Information on the layers which are available and approve for use.",
           c("approved_layers")
)

sd_section("Geoms",
          paste(c("Geoms, short for geometric objects, describe the type of plot you will produce.",
                  "Several of the geoms are accompanied by dedicated stats."), collapse=" "),
          c("geom_crosshair_tern",
            "geom_confidence_tern",
            "geom_density_tern",
            "geom_interpolate_tern",
            "geom_Xline",
            "geom_Xisoprop",
            "geom_errorbarX",
            "geom_smooth_tern",
            "geom_point_swap",
            "geom_mask",
            "geom_label_viewport",
            "geom_text_viewport",
            "geom_mean_ellipse",
            "geom_hex_tern",
            "geom_tri_tern",
            "geom_polygon_closed"
          )
)

sd_section("Annotation",
           "Specialised functions for adding annotations to a plot.",
           c("annotate",
             "annotation_raster_tern")
)

sd_section("Positional Adjustments",
           "Position adjustments can be used to fine tune positioning of objects to achieve effects like dodging, jittering and stacking.",
           c("position_nudge_tern",
             "position_jitter_tern")
)

sd_section("Calculations",
           "Various calculation routines.",
           c("ternary_transformation",
             "mahalanobis_distance")
)

sd_section("Theme Elements",
           "New theme elements, unique to ggtern.",
           c("theme",
             "theme_elements")
)

sd_section("Themes",
           "Complete themes available for use.",
           c("theme_complete",
             "ggtern_themes")
)

sd_section("Convenience Functions",
           "Functions for the rapid customization of plot appearance.",
           c("theme_convenience_functions",
             "theme_arrowlength",
             "theme_gridsontop",
             "theme_bordersontop",
             "theme_clockwise",
             "theme_legend_position",
             "theme_noarrows",
             "theme_nomask",
             "theme_novar_tern",
             "theme_rotate",
             "theme_showgrid",
             "theme_showlabels",
             "theme_showprimary",
             "theme_showtitles",
             "theme_ticksoutside",
             "theme_ticklength",
             "theme_mesh",
             "theme_latex",
             "theme_zoom_X"
             )
)

sd_section("Labels",
           "Ternary-specific Labels.",
           c("ggtern_labels",
             "ggtern_labels_arrow_suffix",
             "label_formatter",
             "breaks_tern",
             'labels_tern')
)

sd_section("Data",
           "The following datasets have been included in the present package.",
           c("data_Feldspar",
             "data_Fragments",
             "data_USDA",
             "data_WhiteCells",
             "data_SkyeLava")
)

sd_section("Legend Keys",
           "Functions related to the renderin of legend keys.",
           c('draw_key_tern'))

sd_section("Arrangement & Saving",
           "The following funcions are useful for saving and printing.",
           c("arrangeGrob",
             "ggsave"))

