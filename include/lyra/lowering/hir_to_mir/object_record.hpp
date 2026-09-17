#pragma once

#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// States, for one class of the source language, the record every object of it
// carries. What hands an object that record as it comes into existence is the
// target's own business and is not this.
//
// A name in one unit may land on a property or a behavior of a class another
// unit declares inside one of its instances (LRM 6.22, 23.9). Such a class is
// on no signature, so the referrer has no name for it and no position it could
// count for itself: where the name lands is settled where the instance is
// known, and what arrives at the access is that answer rather than a name. The
// object is then the only thing that can apply it, because applying it takes
// knowing how the object was laid out -- and for a target that emits another
// language's source, the side that laid it out is that language's compiler.
//
// So the record is what the class says about its own objects, and every part of
// it is something that side alone can answer: where each property it declares
// sits on one, how one is seen as the class it extends, and which body a
// behavior or a declared name reaches. Everything else -- which class declares
// what, how a class extending it gets to that class -- is the same question
// whatever it is spelled in, and is answered by walking rather than by anything
// stated here.
void InstallObjectRecord(
    UnitLowerer& lowerer, mir::ClassId id, mir::Class& cls);

}  // namespace lyra::lowering::hir_to_mir
