// The names an application compiled for a version of the standard calls each
// VPI routine of the object model by (LRM 36.12.2.1, Annex L). Every
// incompatibility LRM 36.12.1 lists is of what a variable or an array of them
// is and what a relation holds of them, so the routines answering a type or a
// relation answer as the version does, and every other one does what the
// routine does.

#include "lyra/runtime/vpi_object.hpp"
#include "lyra/runtime/vpi_routines.hpp"
#include "vpi_user.h"

extern "C" {

auto vpi_compare_objects_1364v1995(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32;
auto vpi_compare_objects_1364v2001(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32;
auto vpi_compare_objects_1364v2005(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32;
auto vpi_compare_objects_1800v2005(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32;
auto vpi_compare_objects_1800v2009(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32;
auto vpi_compare_objects_1800v2012(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32;
auto vpi_get_1364v1995(PLI_INT32 property, vpiHandle object) -> PLI_INT32;
auto vpi_get_1364v2001(PLI_INT32 property, vpiHandle object) -> PLI_INT32;
auto vpi_get_1364v2005(PLI_INT32 property, vpiHandle object) -> PLI_INT32;
auto vpi_get_1800v2005(PLI_INT32 property, vpiHandle object) -> PLI_INT32;
auto vpi_get_1800v2009(PLI_INT32 property, vpiHandle object) -> PLI_INT32;
auto vpi_get_1800v2012(PLI_INT32 property, vpiHandle object) -> PLI_INT32;
auto vpi_get_str_1364v1995(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8*;
auto vpi_get_str_1364v2001(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8*;
auto vpi_get_str_1364v2005(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8*;
auto vpi_get_str_1800v2005(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8*;
auto vpi_get_str_1800v2009(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8*;
auto vpi_get_str_1800v2012(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8*;
auto vpi_handle_1364v1995(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_handle_1364v2001(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_handle_1364v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_handle_1800v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_handle_1800v2009(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_handle_1800v2012(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_handle_by_index_1364v1995(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle;
auto vpi_handle_by_index_1364v2001(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle;
auto vpi_handle_by_index_1364v2005(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle;
auto vpi_handle_by_index_1800v2005(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle;
auto vpi_handle_by_index_1800v2009(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle;
auto vpi_handle_by_index_1800v2012(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle;
auto vpi_handle_by_multi_index_1364v1995(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle;
auto vpi_handle_by_multi_index_1364v2001(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle;
auto vpi_handle_by_multi_index_1364v2005(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle;
auto vpi_handle_by_multi_index_1800v2005(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle;
auto vpi_handle_by_multi_index_1800v2009(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle;
auto vpi_handle_by_multi_index_1800v2012(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle;
auto vpi_handle_by_name_1364v1995(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle;
auto vpi_handle_by_name_1364v2001(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle;
auto vpi_handle_by_name_1364v2005(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle;
auto vpi_handle_by_name_1800v2005(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle;
auto vpi_handle_by_name_1800v2009(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle;
auto vpi_handle_by_name_1800v2012(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle;
auto vpi_iterate_1364v1995(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_iterate_1364v2001(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_iterate_1364v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_iterate_1800v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_iterate_1800v2009(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_iterate_1800v2012(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle;
auto vpi_scan_1364v1995(vpiHandle iterator) -> vpiHandle;
auto vpi_scan_1364v2001(vpiHandle iterator) -> vpiHandle;
auto vpi_scan_1364v2005(vpiHandle iterator) -> vpiHandle;
auto vpi_scan_1800v2005(vpiHandle iterator) -> vpiHandle;
auto vpi_scan_1800v2009(vpiHandle iterator) -> vpiHandle;
auto vpi_scan_1800v2012(vpiHandle iterator) -> vpiHandle;

void vpi_get_value_1364v1995(vpiHandle expr, p_vpi_value value_p);
auto vpi_put_value_1364v1995(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle;
void vpi_get_value_1364v2001(vpiHandle expr, p_vpi_value value_p);
auto vpi_put_value_1364v2001(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle;
void vpi_get_value_1364v2005(vpiHandle expr, p_vpi_value value_p);
auto vpi_put_value_1364v2005(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle;
void vpi_get_value_1800v2005(vpiHandle expr, p_vpi_value value_p);
auto vpi_put_value_1800v2005(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle;
void vpi_get_value_1800v2009(vpiHandle expr, p_vpi_value value_p);
auto vpi_put_value_1800v2009(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle;
void vpi_get_value_1800v2012(vpiHandle expr, p_vpi_value value_p);
auto vpi_put_value_1800v2012(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle;

auto vpi_compare_objects_1364v1995(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32 {
  return vpi_compare_objects(object1, object2);
}

auto vpi_compare_objects_1364v2001(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32 {
  return vpi_compare_objects(object1, object2);
}

auto vpi_compare_objects_1364v2005(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32 {
  return vpi_compare_objects(object1, object2);
}

auto vpi_compare_objects_1800v2005(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32 {
  return vpi_compare_objects(object1, object2);
}

auto vpi_compare_objects_1800v2009(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32 {
  return vpi_compare_objects(object1, object2);
}

auto vpi_compare_objects_1800v2012(vpiHandle object1, vpiHandle object2)
    -> PLI_INT32 {
  return vpi_compare_objects(object1, object2);
}

auto vpi_get_1364v1995(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return lyra::runtime::VpiGet(
      lyra::runtime::VpiMode::k1364v1995, property, object);
}

auto vpi_get_1364v2001(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return lyra::runtime::VpiGet(
      lyra::runtime::VpiMode::k1364v2001, property, object);
}

auto vpi_get_1364v2005(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return lyra::runtime::VpiGet(
      lyra::runtime::VpiMode::k1364v2005, property, object);
}

auto vpi_get_1800v2005(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return lyra::runtime::VpiGet(
      lyra::runtime::VpiMode::k1800v2005, property, object);
}

auto vpi_get_1800v2009(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return lyra::runtime::VpiGet(
      lyra::runtime::VpiMode::k1800v2009, property, object);
}

auto vpi_get_1800v2012(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return lyra::runtime::VpiGet(
      lyra::runtime::VpiMode::k1800v2012, property, object);
}

auto vpi_get_str_1364v1995(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return lyra::runtime::VpiGetStr(
      lyra::runtime::VpiMode::k1364v1995, property, object);
}

auto vpi_get_str_1364v2001(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return lyra::runtime::VpiGetStr(
      lyra::runtime::VpiMode::k1364v2001, property, object);
}

auto vpi_get_str_1364v2005(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return lyra::runtime::VpiGetStr(
      lyra::runtime::VpiMode::k1364v2005, property, object);
}

auto vpi_get_str_1800v2005(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return lyra::runtime::VpiGetStr(
      lyra::runtime::VpiMode::k1800v2005, property, object);
}

auto vpi_get_str_1800v2009(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return lyra::runtime::VpiGetStr(
      lyra::runtime::VpiMode::k1800v2009, property, object);
}

auto vpi_get_str_1800v2012(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return lyra::runtime::VpiGetStr(
      lyra::runtime::VpiMode::k1800v2012, property, object);
}

auto vpi_handle_1364v1995(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_handle(type, refHandle);
}

auto vpi_handle_1364v2001(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_handle(type, refHandle);
}

auto vpi_handle_1364v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_handle(type, refHandle);
}

auto vpi_handle_1800v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_handle(type, refHandle);
}

auto vpi_handle_1800v2009(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_handle(type, refHandle);
}

auto vpi_handle_1800v2012(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_handle(type, refHandle);
}

auto vpi_handle_by_index_1364v1995(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle {
  return vpi_handle_by_index(object, indx);
}

auto vpi_handle_by_index_1364v2001(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle {
  return vpi_handle_by_index(object, indx);
}

auto vpi_handle_by_index_1364v2005(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle {
  return vpi_handle_by_index(object, indx);
}

auto vpi_handle_by_index_1800v2005(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle {
  return vpi_handle_by_index(object, indx);
}

auto vpi_handle_by_index_1800v2009(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle {
  return vpi_handle_by_index(object, indx);
}

auto vpi_handle_by_index_1800v2012(vpiHandle object, PLI_INT32 indx)
    -> vpiHandle {
  return vpi_handle_by_index(object, indx);
}

auto vpi_handle_by_multi_index_1364v1995(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle {
  return vpi_handle_by_multi_index(obj, num_index, index_array);
}

auto vpi_handle_by_multi_index_1364v2001(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle {
  return vpi_handle_by_multi_index(obj, num_index, index_array);
}

auto vpi_handle_by_multi_index_1364v2005(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle {
  return vpi_handle_by_multi_index(obj, num_index, index_array);
}

auto vpi_handle_by_multi_index_1800v2005(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle {
  return vpi_handle_by_multi_index(obj, num_index, index_array);
}

auto vpi_handle_by_multi_index_1800v2009(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle {
  return vpi_handle_by_multi_index(obj, num_index, index_array);
}

auto vpi_handle_by_multi_index_1800v2012(
    vpiHandle obj, PLI_INT32 num_index, PLI_INT32* index_array) -> vpiHandle {
  return vpi_handle_by_multi_index(obj, num_index, index_array);
}

auto vpi_handle_by_name_1364v1995(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle {
  return vpi_handle_by_name(name, scope);
}

auto vpi_handle_by_name_1364v2001(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle {
  return vpi_handle_by_name(name, scope);
}

auto vpi_handle_by_name_1364v2005(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle {
  return vpi_handle_by_name(name, scope);
}

auto vpi_handle_by_name_1800v2005(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle {
  return vpi_handle_by_name(name, scope);
}

auto vpi_handle_by_name_1800v2009(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle {
  return vpi_handle_by_name(name, scope);
}

auto vpi_handle_by_name_1800v2012(PLI_BYTE8* name, vpiHandle scope)
    -> vpiHandle {
  return vpi_handle_by_name(name, scope);
}

auto vpi_iterate_1364v1995(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return lyra::runtime::VpiIterate(
      lyra::runtime::VpiMode::k1364v1995, type, refHandle);
}

auto vpi_iterate_1364v2001(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return lyra::runtime::VpiIterate(
      lyra::runtime::VpiMode::k1364v2001, type, refHandle);
}

auto vpi_iterate_1364v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return lyra::runtime::VpiIterate(
      lyra::runtime::VpiMode::k1364v2005, type, refHandle);
}

auto vpi_iterate_1800v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return lyra::runtime::VpiIterate(
      lyra::runtime::VpiMode::k1800v2005, type, refHandle);
}

auto vpi_iterate_1800v2009(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return lyra::runtime::VpiIterate(
      lyra::runtime::VpiMode::k1800v2009, type, refHandle);
}

auto vpi_iterate_1800v2012(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return lyra::runtime::VpiIterate(
      lyra::runtime::VpiMode::k1800v2012, type, refHandle);
}

auto vpi_scan_1364v1995(vpiHandle iterator) -> vpiHandle {
  return vpi_scan(iterator);
}

auto vpi_scan_1364v2001(vpiHandle iterator) -> vpiHandle {
  return vpi_scan(iterator);
}

auto vpi_scan_1364v2005(vpiHandle iterator) -> vpiHandle {
  return vpi_scan(iterator);
}

auto vpi_scan_1800v2005(vpiHandle iterator) -> vpiHandle {
  return vpi_scan(iterator);
}

auto vpi_scan_1800v2009(vpiHandle iterator) -> vpiHandle {
  return vpi_scan(iterator);
}

auto vpi_scan_1800v2012(vpiHandle iterator) -> vpiHandle {
  return vpi_scan(iterator);
}

void vpi_get_value_1364v1995(vpiHandle expr, p_vpi_value value_p) {
  vpi_get_value(expr, value_p);
}

auto vpi_put_value_1364v1995(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle {
  return vpi_put_value(object, value_p, time_p, flags);
}

void vpi_get_value_1364v2001(vpiHandle expr, p_vpi_value value_p) {
  vpi_get_value(expr, value_p);
}

auto vpi_put_value_1364v2001(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle {
  return vpi_put_value(object, value_p, time_p, flags);
}

void vpi_get_value_1364v2005(vpiHandle expr, p_vpi_value value_p) {
  vpi_get_value(expr, value_p);
}

auto vpi_put_value_1364v2005(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle {
  return vpi_put_value(object, value_p, time_p, flags);
}

void vpi_get_value_1800v2005(vpiHandle expr, p_vpi_value value_p) {
  vpi_get_value(expr, value_p);
}

auto vpi_put_value_1800v2005(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle {
  return vpi_put_value(object, value_p, time_p, flags);
}

void vpi_get_value_1800v2009(vpiHandle expr, p_vpi_value value_p) {
  vpi_get_value(expr, value_p);
}

auto vpi_put_value_1800v2009(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle {
  return vpi_put_value(object, value_p, time_p, flags);
}

void vpi_get_value_1800v2012(vpiHandle expr, p_vpi_value value_p) {
  vpi_get_value(expr, value_p);
}

auto vpi_put_value_1800v2012(
    vpiHandle object, p_vpi_value value_p, p_vpi_time time_p, PLI_INT32 flags)
    -> vpiHandle {
  return vpi_put_value(object, value_p, time_p, flags);
}

}  // extern "C"
