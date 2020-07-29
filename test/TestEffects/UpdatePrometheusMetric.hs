module TestEffects.UpdatePrometheusMetric
    ( runUpdatePrometheusMetricsFake
    )
where

import qualified Polysemy as P
import qualified Polysemy.Output as P
import Effects.UpdatePrometheusMetric(UpdatePrometheusMetric(..))

runUpdatePrometheusMetricsFake :: P.Sem (UpdatePrometheusMetric:r) a -> P.Sem (P.Output String:r) a
runUpdatePrometheusMetricsFake = 
        P.reinterpret $ \case
            UpdateEnergyConsumedTariff2 _     -> P.output "UpdateEnergyConsumedTariff2"
            UpdateEnergyConsumedTariff1 _     -> P.output "UpdateEnergyConsumedTariff1"
            UpdateEnergyReturnedTariff1 _     -> P.output "UpdateEnergyReturnedTariff1"
            UpdateEnergyReturnedTariff2 _     -> P.output "UpdateEnergyReturnedTariff2"
            UpdateActualTariffIndicator _     -> P.output "UpdateActualTariffIndicator"
            UpdateActualPowerConsumption _    -> P.output "UpdateActualPowerConsumption"
            UpdateActualPowerReturned _       -> P.output "UpdateActualPowerReturned"
            UpdateNumberOfPowerFailures _     -> P.output "UpdateNumberOfPowerFailures"
            UpdateNumberOfPowerLongFailures _ -> P.output "UpdateNumberOfPowerLongFailures"
            UpdateActualCurrentConsumption _  -> P.output "UpdateActualCurrentConsumption"
            UpdateGasConsumption _ _          -> P.output "UpdateGasConsumption"
            UpdateNothing                     -> P.output "UpdateNothing"