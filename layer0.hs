module PhysicalLayer where

import Data.Word (Word32, Word64)
import Data.Text (Text)

-- ==========================================
-- Primitive Types & Enums
-- ==========================================

type Wattage = Word32
type SerialNumber = Text
type UnitID = Text
type SlotIndex = Word32 -- Rack Unit position (e.g., 1 to 42)

data Vendor 
    = Schneider 
    | Huawei 
    | Dell 
    | HP 
    | Cisco
    | Intel
    | Samsung
    deriving (Show, Eq)

data PowerState 
    = On 
    | Off 
    | Error
    deriving (Show, Eq)

data StorageType 
    = HDD 
    | SSD 
    | NVMe
    deriving (Show, Eq)

data InterfaceType 
    = RJ45 
    | SFP_Plus -- 10G
    | QSFP28   -- 100G
    deriving (Show, Eq)

-- ==========================================
-- Component Layer (Parts of a Machine/Device)
-- ==========================================

-- | Physical Disk / Storage Device
data Disk = Disk
    { diskSerial :: SerialNumber
    , diskVendor :: Vendor
    , diskType :: StorageType
    , diskCapacityGB :: Word32
    } deriving (Show, Eq)

-- | Network Interface (Physical Port)
data NetworkInterface = NetworkInterface
    { nicMacAddress :: Text
    , nicSpeedMbps :: Word32
    , nicType :: InterfaceType
    } deriving (Show, Eq)

-- | Power Supply Unit (Component)
data PSU = PSU
    { psuSerial :: SerialNumber
    , psuMaxWatts :: Wattage
    , psuEfficient :: Float -- 0.0 to 1.0
    } deriving (Show, Eq)

-- ==========================================
-- Device Layer (Rack-mountable equipment)
-- ==========================================

-- | Physical Server / Machine
data Machine = Machine
    { machineSerial :: SerialNumber
    , machineVendor :: Vendor
    , machineModel :: Text
    , machineCpuCores :: Word32
    , machineRamGB :: Word32
    , machineDisks :: [Disk]
    , machineNics :: [NetworkInterface]
    , machinePsus :: [PSU]
    , machineHeightU :: Word32 -- Height in Rack Units (e.g., 1U, 2U)
    } deriving (Show, Eq)

-- | Network Switch (Top of Rack / Aggregation)
data Switch = Switch
    { switchSerial :: SerialNumber
    , switchVendor :: Vendor
    , switchTotalPorts :: Word32
    , switchUplinkSpeed :: Word32
    , switchPowerDraw :: Wattage
    } deriving (Show, Eq)

-- ==========================================
-- Infrastructure Layer
-- ==========================================

-- | Rack Power Distribution Unit
data RackPDU = RackPDU
    { pduSerial :: SerialNumber
    , pduMaxWatts :: Wattage
    , pduSockets :: Word32
    } deriving (Show, Eq)

-- | Physical Rack
data Rack = Rack
    { rackId :: UnitID
    , rackTotalUnits :: Word32 -- Standard is 42U
    , rackPdus :: [RackPDU]
    , rackMachines :: [(SlotIndex, Machine)] -- Mounted Machines
    , rackSwitches :: [(SlotIndex, Switch)]  -- Mounted Switches
    } deriving (Show, Eq)

-- | External Connectivity
data ISP = ISP
    { ispName :: Text
    , ispBandwidthMbps :: Word64
    , ispDemarcationPoint :: UnitID
    } deriving (Show, Eq)

-- | The Datacenter Facility
data Datacenter = Datacenter
    { dcName :: Text
    , dcLocation :: Text
    , dcRacks :: [Rack]
    , dcIsps :: [ISP]
    , dcTotalPowerCapacity :: Wattage
    } deriving (Show, Eq)

{- 
    INVARIANTS (Conceptual - to be enforced by validation logic)
    
    1. Datacenter:
       - len racks > 0
       - len dcIsps > 0
       - total_power_draw(racks) <= dcTotalPowerCapacity

    2. Rack:
       - sum(machine.height + switch.height) <= rackTotalUnits
       - All SlotIndices must be unique and within [1, rackTotalUnits]
       - sum(machine.power) <= sum(pdu.maxWatts)

    3. Machine:
       - len disks > 0 (usually)
       - len nics > 0
-}
