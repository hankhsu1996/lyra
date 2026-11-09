#pragma once

#include <map>
#include <optional>

#include "lyra/hir/class_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// The run's registry of covergroup types and its database (LRM 19.9).
struct CoverageRegistryMembers {
  ClassId cls;
  ClassMethodTarget type_named;
  ClassMethodTarget claim_database_writing;
  ClassMethodTarget write_database;
  ClassMethodTarget get_coverage;
  ClassMethodTarget set_coverage_db_name;
  ClassMethodTarget load_coverage_db;

  auto operator==(const CoverageRegistryMembers&) const -> bool = default;
};

// A covergroup type's record (LRM 19.11.3).
struct CovergroupTypeMembers {
  ClassId cls;
  ClassMethodTarget make_instance;
  ClassMethodTarget declare_coverpoint;
  ClassMethodTarget declare_cross;
  ClassMethodTarget declare_replaced;
  ClassMethodTarget get_coverage;
  ClassMethodTarget coverage;
  ClassMethodTarget get_item_coverage;
  ClassMethodTarget item_coverage;
  ClassPropertyTarget declared;
  ClassPropertyTarget type_option;
  ClassPropertyTarget coverpoint_type_options;
  ClassPropertyTarget cross_type_options;

  auto operator==(const CovergroupTypeMembers&) const -> bool = default;
};

// An instance's record (LRM 19.8).
struct CovergroupInstanceMembers {
  ClassId cls;
  ClassMethodTarget strobed;
  ClassMethodTarget claim_strobe;
  ClassMethodTarget strobe_taken;
  ClassMethodTarget get_inst_coverage;
  ClassMethodTarget inst_coverage;
  ClassMethodTarget set_inst_name;
  ClassMethodTarget start;
  ClassMethodTarget stop;
  ClassPropertyTarget option;
  ClassPropertyTarget block_event;
  ClassPropertyTarget collecting;

  auto operator==(const CovergroupInstanceMembers&) const -> bool = default;
};

// The library class `coverage_item`: what a coverpoint and a cross have alike
// (LRM 19.8).
struct CoverageItemMembers {
  ClassId cls;
  ClassMethodTarget get_inst_coverage;
  ClassMethodTarget inst_coverage;
  ClassMethodTarget start;
  ClassMethodTarget stop;

  auto operator==(const CoverageItemMembers&) const -> bool = default;
};

// The library class `coverage_coverpoint`: what every coverpoint has, whatever
// class counts its values.
struct CoverageCoverpointMembers {
  ClassId cls;
  ClassMethodTarget all_numbers;
  ClassMethodTarget numbers_of_construct;
  ClassMethodTarget skip;
  ClassPropertyTarget option;

  auto operator==(const CoverageCoverpointMembers&) const -> bool = default;
};

// A cross (LRM 19.6).
struct CoverageCrossMembers {
  ClassId cls;
  ClassMethodTarget cross_coverpoint;
  ClassMethodTarget add_bin;
  ClassMethodTarget finish;
  ClassMethodTarget sample;
  ClassMethodTarget gate;
  ClassMethodTarget select_all;
  ClassMethodTarget select_numbers;
  ClassMethodTarget select_outside;
  ClassMethodTarget both_numbers;
  ClassMethodTarget begin_filter;
  ClassMethodTarget candidate_tuples;
  ClassMethodTarget offer;
  ClassMethodTarget component;
  ClassMethodTarget end_filter;
  ClassMethodTarget value_component;
  ClassMethodTarget end_value_tuple;
  ClassPropertyTarget option;

  auto operator==(const CoverageCrossMembers&) const -> bool = default;
};

// A cross's selection of its tuples (LRM 19.6.1).
struct CoverageSelectionMembers {
  ClassId cls;
  ClassMethodTarget both;
  ClassMethodTarget either;

  auto operator==(const CoverageSelectionMembers&) const -> bool = default;
};

// What only the class counting an integral coverpoint's values has: transition
// bins (LRM 19.5.2), wildcard bins (LRM 19.5.4), a `with` filter's values (LRM
// 19.5.1.1), an enumeration's named values (LRM 19.5.3), and the values a
// cross's `with` filter walks (LRM 19.6.1.2), none of which a real coverpoint
// takes.
struct IntegralCoverpointClassMembers {
  ClassMethodTarget add_wildcard;
  ClassMethodTarget add_default_sequence_bin;
  ClassMethodTarget begin_transition_bins;
  ClassMethodTarget begin_sequence;
  ClassMethodTarget begin_step;
  ClassMethodTarget end_step;
  ClassMethodTarget end_sequence;
  ClassMethodTarget end_transition_bins;
  ClassMethodTarget pending_values;
  ClassMethodTarget keep_value;
  ClassMethodTarget name_enumeration_value;
  ClassMethodTarget values_in_bin;

  auto operator==(const IntegralCoverpointClassMembers&) const
      -> bool = default;
};

// What the class counting one coverpoint's values has, which the coverpoint is
// an object of: the integral coverpoint at the coverpoint's type, or the real
// one (LRM 19.5.7).
struct CoverpointClassMembers {
  ClassMethodTarget add_value;
  ClassMethodTarget add_every_value;
  ClassMethodTarget add_range;
  ClassMethodTarget add_range_from_least;
  ClassMethodTarget add_range_to_greatest;
  ClassMethodTarget add_tolerance;
  ClassMethodTarget add_default_bin;
  ClassMethodTarget begin_bins;
  ClassMethodTarget end_bins;
  ClassMethodTarget finish;
  ClassMethodTarget sample;
  ClassMethodTarget gate;
  ClassMethodTarget begin_selection;
  ClassMethodTarget selected_bins;
  ClassMethodTarget numbers_containing;
  ClassMethodTarget text_of;
  std::optional<IntegralCoverpointClassMembers> integral;

  auto operator==(const CoverpointClassMembers&) const -> bool = default;
};

// A value of one of the library's enumerations, and the enumeration's type.
struct LibraryEnumerator {
  TypeId type;
  IntegralConstant value;

  auto operator==(const LibraryEnumerator&) const -> bool = default;
};

// What a bin construct counts toward, how many bins it makes, and how a
// transition step repeats (LRM 19.5.1, 19.5.2, 19.5.5, 19.5.6).
struct CoverageEnumerators {
  LibraryEnumerator bins;
  LibraryEnumerator ignore_bins;
  LibraryEnumerator illegal_bins;
  LibraryEnumerator one_bin;
  LibraryEnumerator bin_per_value;
  LibraryEnumerator fixed_bins;
  LibraryEnumerator once;
  LibraryEnumerator consecutive;
  LibraryEnumerator go_to;
  LibraryEnumerator nonconsecutive;

  auto operator==(const CoverageEnumerators&) const -> bool = default;
};

// Every member of the coverage library a covergroup's class is written over in
// the step to MIR (LRM 19), as one unit names it: each resolved once, where the
// unit names the library, to the identity the unit holds it by, and named as
// the library names it.
struct CoverageLibraryMembers {
  CoverageRegistryMembers registry;
  CovergroupTypeMembers group_type;
  CovergroupInstanceMembers group_instance;
  CoverageItemMembers item;
  CoverageCoverpointMembers coverpoint;
  CoverageCrossMembers cross;
  CoverageSelectionMembers selection;
  CoverageEnumerators enumerators;
  // Each class counting one of the unit's own coverpoints.
  std::map<ClassId, CoverpointClassMembers> coverpoint_classes;

  auto operator==(const CoverageLibraryMembers&) const -> bool = default;
};

}  // namespace lyra::hir
