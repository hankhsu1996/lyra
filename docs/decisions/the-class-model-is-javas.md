# SystemVerilog's class model is Java's

Status: Accepted, 2026-11-22.

## Requirement

A class of a SystemVerilog design behaves as the standard says, including in what the standard does
not spell out clause by clause. Where the standard states class semantics it states Java's. Classes
are garbage collected "as in Java" (LRM 8.7); a derived `new` calls its base's, then initializes its
own properties, then runs its body (LRM 8.7); a virtual method called from a constructor is the one
the object's class implements, and "properties the method refers to may not have been initialized"
(LRM 8.17 NOTE 2), which is JLS 12.5's rule and its example 12.5-2 in another language; and an
object lives while anything refers to it (LRM 8.29). The class half borrowed some syntax from C++
(`::`), but none of these meanings is C++'s.

## Choice

**Every class question is answered as Java answers it.** Each SystemVerilog form is written as its
Java form and takes the answer of the Java Language Specification and of javac before any single
clause read alone, any other language's or any tool's output; only what SystemVerilog's own text
grants on top of Java is added. Where the standard leaves a class question open, Java's answer comes
before any other language's. A form whose syntax SystemVerilog took from C++ still takes Java's
meaning.

## Alternatives turned down

- **C++'s answer, which the C++ backend's rendering brings with it.** It fails LRM 8.17 NOTE 2,
  since a C++ object takes its class one base at a time while constructors run, so a base
  constructor dispatches as the base. It also fails LRM 8.29, since C++ unwinds a construction left
  partway by freeing the object a handle may still name. The failure rests on rendering an SV
  constructor as a C++ constructor carrying C++'s rules into a program whose MIR never stated them.
  Lyra built objects that way, and a base constructor dispatched as the base, a cast inside it
  failed, and LRM 8.7's own example compiled on neither backend.
- **Each class question answered clause by clause from the standard alone.** It fails the questions
  the standard leaves open, which it answers nowhere, and invites each to be answered by whatever
  the backend's host language does.
- **A simulator's output as the reference.** It fails a reading argued from the standard, since an
  answer leaning on one tool inherits that tool's reading.

## Consequences across layers

- An object is created whole, with its class fixed once at creation, and then constructed, as JLS
  12.5 does and every runtime with a collector does at allocation
  ([`object_model.md`](../architecture/object_model.md)); the reference model is Java's
  ([object-model](object-model.md), [`lifetime.md`](../architecture/lifetime.md)).
- The rule that the execution backend realizes what clang makes of the C++ backend's rendering holds
  only where that rendering adds no C++ semantics of its own; construction, dispatch during it and
  lifetime are read off the standard and Java, never off clang.
- HIR carries the source's class vocabulary, and every layer from HIR-to-MIR down to both backends
  and the runtime states Java's meaning for it.
