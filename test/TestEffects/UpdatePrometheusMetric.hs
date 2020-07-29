module TestEffects.UpdatePrometheusMetric
    ( runUpdatePrometheusMetricsFake
    )
where

import qualified Polysemy as P
import qualified Polysemy.Output as P
import Effects.UpdatePrometheusMetric(UpdatePrometheusMetric(..))

import Debug.Trace(trace)

runUpdatePrometheusMetricsFake :: P.Sem (UpdatePrometheusMetric:r) a -> P.Sem (P.Output String:r) a
runUpdatePrometheusMetricsFake = 
        P.reinterpret $ \case
            UpdateEnergyConsumedTariff2 _     -> trace ("updating the shizzle!") $ P.output "UpdateEnergyConsumedTariff2"
            UpdateEnergyConsumedTariff1 _     -> trace ("updating the shizzle!") $ P.output "UpdateEnergyConsumedTariff1"
            UpdateEnergyReturnedTariff1 _     -> trace ("updating the shizzle!") $ P.output "UpdateEnergyReturnedTariff1"
            UpdateEnergyReturnedTariff2 _     -> trace ("updating the shizzle!") $ P.output "UpdateEnergyReturnedTariff2"
            UpdateActualTariffIndicator _     -> trace ("updating the shizzle!") $ P.output "UpdateActualTariffIndicator"
            UpdateActualPowerConsumption _    -> trace ("updating the shizzle!") $ P.output "UpdateActualPowerConsumption"
            UpdateActualPowerReturned _       -> trace ("updating the shizzle!") $ P.output "UpdateActualPowerReturned"
            UpdateNumberOfPowerFailures _     -> trace ("updating the shizzle!") $ P.output "UpdateNumberOfPowerFailures"
            UpdateNumberOfPowerLongFailures _ -> trace ("updating the shizzle!") $ P.output "UpdateNumberOfPowerLongFailures"
            UpdateActualCurrentConsumption _  -> trace ("updating the shizzle!") $ P.output "UpdateActualCurrentConsumption"
            UpdateGasConsumption _ _          -> trace ("updating the shizzle!") $ P.output "UpdateGasConsumption"
            UpdateNothing                     -> trace ("updating the shizzle!") $ P.output "UpdateNothing"