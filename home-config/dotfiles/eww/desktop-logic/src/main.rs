//! Set of little desktop logic handlers, primarily for adding
//! functionality on top of eww and sway

#![warn(
    clippy::pedantic,
    clippy::allow_attributes,
    clippy::allow_attributes_without_reason,
    clippy::arithmetic_side_effects,
    clippy::as_conversions,
    clippy::as_pointer_underscore,
    clippy::as_underscore,
    clippy::assertions_on_result_states,
    clippy::clone_on_ref_ptr,
    clippy::dbg_macro,
    clippy::decimal_literal_representation,
    clippy::empty_enum_variants_with_brackets,
    clippy::empty_structs_with_brackets,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::expect_used,
    clippy::filetype_is_file,
    clippy::float_cmp_const,
    clippy::fn_to_numeric_cast_any,
    clippy::if_then_some_else_none,
    clippy::impl_trait_in_params,
    clippy::indexing_slicing,
    clippy::infinite_loop,
    clippy::large_include_file,
    clippy::lossy_float_literal,
    clippy::map_err_ignore,
    clippy::map_with_unused_argument_over_ranges,
    clippy::mem_forget,
    clippy::min_ident_chars,
    clippy::missing_assert_message,
    clippy::missing_asserts_for_indexing,
    clippy::missing_docs_in_private_items,
    clippy::missing_inline_in_public_items,
    clippy::mixed_read_write_in_expression,
    clippy::module_name_repetitions,
    clippy::multiple_unsafe_ops_per_block,
    clippy::mutex_atomic,
    clippy::mutex_integer,
    clippy::needless_raw_strings,
    clippy::non_zero_suggestions,
    clippy::panic,
    clippy::partial_pub_fields,
    clippy::pathbuf_init_then_push,
    clippy::pattern_type_mismatch,
    clippy::precedence_bits,
    clippy::pub_without_shorthand,
    clippy::rc_buffer,
    clippy::rc_mutex,
    // clippy::redundant_test_prefix,
    clippy::redundant_type_annotations,
    clippy::ref_patterns,
    clippy::renamed_function_params,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::return_and_then,
    clippy::self_named_module_files,
    clippy::semicolon_inside_block,
    clippy::shadow_unrelated,
    clippy::single_char_lifetime_names,
    clippy::str_to_string,
    clippy::string_lit_chars_any,
    clippy::string_slice,
    clippy::suspicious_xor_used_as_pow,
    clippy::tests_outside_test_module,
    clippy::todo,
    clippy::try_err,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::unnecessary_safety_comment,
    clippy::unnecessary_safety_doc,
    clippy::unneeded_field_pattern,
    clippy::unseparated_literal_suffix,
    clippy::unused_result_ok,
    clippy::unused_trait_names,
    clippy::unwrap_used,
    clippy::use_debug,
    clippy::verbose_file_reads
)]

mod idle_inhibit;
mod notifications;

use idle_inhibit::IdleInhibitor;
use notifications::NotificationHandler;
use zbus::connection;

#[tokio::main]
async fn main() -> zbus::Result<()> {
    env_logger::init();

    let inhibitor = IdleInhibitor::try_new().await?;

    let _connection = connection::Builder::session()?
        .name("net.tlater.DesktopLogic")?
        .serve_at("/net/tlater/desktoplogic/idleinhibitor", inhibitor)?
        .build()
        .await?;

    let notification_handler = NotificationHandler::try_new();
    notification_handler.run().await;

    Ok(())
}
