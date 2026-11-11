// The names an application compiled for a version of the standard calls each
// VPI routine of the object model by (LRM 36.12.2.1, Annex L). No version
// changed what these routines answer of the objects Lyra reaches -- every
// incompatibility LRM 36.12.1 lists is of variables and arrays of them -- so
// each does what the routine does.

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
  return vpi_get(property, object);
}

auto vpi_get_1364v2001(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return vpi_get(property, object);
}

auto vpi_get_1364v2005(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return vpi_get(property, object);
}

auto vpi_get_1800v2005(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return vpi_get(property, object);
}

auto vpi_get_1800v2009(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return vpi_get(property, object);
}

auto vpi_get_1800v2012(PLI_INT32 property, vpiHandle object) -> PLI_INT32 {
  return vpi_get(property, object);
}

auto vpi_get_str_1364v1995(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return vpi_get_str(property, object);
}

auto vpi_get_str_1364v2001(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return vpi_get_str(property, object);
}

auto vpi_get_str_1364v2005(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return vpi_get_str(property, object);
}

auto vpi_get_str_1800v2005(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return vpi_get_str(property, object);
}

auto vpi_get_str_1800v2009(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return vpi_get_str(property, object);
}

auto vpi_get_str_1800v2012(PLI_INT32 property, vpiHandle object) -> PLI_BYTE8* {
  return vpi_get_str(property, object);
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
  return vpi_iterate(type, refHandle);
}

auto vpi_iterate_1364v2001(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_iterate(type, refHandle);
}

auto vpi_iterate_1364v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_iterate(type, refHandle);
}

auto vpi_iterate_1800v2005(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_iterate(type, refHandle);
}

auto vpi_iterate_1800v2009(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_iterate(type, refHandle);
}

auto vpi_iterate_1800v2012(PLI_INT32 type, vpiHandle refHandle) -> vpiHandle {
  return vpi_iterate(type, refHandle);
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

}  // extern "C"
