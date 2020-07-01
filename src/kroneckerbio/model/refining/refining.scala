/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

package object refining {
  // Pattern types
  type ModelRulePattern = ModelPatternClass[ModelReactantGraph]
  type ModelObservedPattern = ModelPatternClass[ModelStaticGraph]

  // Graph Types
  type ModelGraph = ModelGraphClass[ModelMonomer,ModelBoundEdge]
  type ModelCoreGraph = ModelGraphClass[ModelMonomer,ModelBoundEdge]
  type ModelReactantGraph = ModelGraphClass[ModelReactantMonomer,ModelReactantBoundEdge]
  type ModelProductGraph = ModelGraphClass[ModelProductMonomer,ModelProductBoundEdge]
  type ModelStaticGraph = ModelGraphClass[ModelStaticMonomer,ModelStaticBoundEdge]

  // Monomer types
  type ModelMonomer = ModelMonomerClass[ModelStateSite,ModelEdgeSite,ModelCompartmentSite]
  type ModelReactantMonomer = ModelMonomerClass[ModelReactantStateSite, ModelReactantEdgeSite, ModelReactantCompartmentSite]
  type ModelProductMonomer = ModelMonomerClass[ModelProductStateSite, ModelProductEdgeSite, ModelProductCompartmentSite]
  type ModelConsumedMonomer = ModelMonomerClass[ModelConsumedStateSite, ModelConsumedEdgeSite, ModelConsumedCompartmentSite]
  type ModelProducedMonomer = ModelMonomerClass[ModelProducedStateSite, ModelProducedEdgeSite, ModelProducedCompartmentSite]
  type ModelConservedMonomer = ModelMonomerClass[ModelConservedStateSite, ModelConservedEdgeSite, ModelConservedCompartmentSite]
  type ModelStaticMonomer = ModelMonomerClass[ModelStaticStateSite, ModelStaticEdgeSite, ModelStaticCompartmentSite]
}
