module OSLayer where

import Data.Text (Text)
import Data.Word (Word32)
import PhysicalLayer (SerialNumber)

-- ==========================================
-- Layer 1: Host OS & System Software
-- ==========================================

-- | Types of Operating Systems running on bare metal
data OSType 
    = Linux LinuxDistro
    | WindowsServer
    | Hypervisor -- e.g., ESXi, Xen
    | FirewallOS -- e.g., pfSense, vendor specific
    deriving (Show, Eq)

data LinuxDistro 
    = Ubuntu
    | RHEL
    | CentOS
    | Debian
    deriving (Show, Eq)

-- | Operational State of the OS
data SystemState
    = Active
    | Inactive
    | MaintenanceMode
    | Failed
    deriving (Show, Eq)

-- ==========================================
-- Interfaces & Networking (Layer 1 View)
-- ==========================================

-- | Logical Interface definition within the OS
-- This binds to the Physical Layer NetworkInterface
data LogicalInterface = LogicalInterface
    { ifName :: Text              -- e.g., "eth0", "bond0"
    , ifIpAddress :: Text         -- IPv4/IPv6 Address (e.g., "10.0.0.1/24")
    , ifMacRef :: Text            -- Matches PhysicalLayer.NetworkInterface.nicMacAddress
    , ifIsUp :: Bool
    } deriving (Show, Eq)

-- ==========================================
-- Processes & Services
-- ==========================================

-- | Classification of processes running on the host
data ProcessType
    = SystemDaemon        -- e.g., sshd, systemd
    | OpenStackService    -- e.g., nova-compute, neutron-agent
    | StorageService      -- e.g., ceph-osd
    | NetworkService      -- e.g., ovs-vswitchd
    | UserApplication
    deriving (Show, Eq)

-- | A running process on the OS
data Process = Process
    { procPid :: Word32
    , procName :: Text
    , procType :: ProcessType
    , procUser :: Text
    , procState :: Text   -- "Running", "Sleeping", "Zombie"
    } deriving (Show, Eq)

-- ==========================================
-- The Host System
-- ==========================================

-- | Represents the Software Layer running directly on Layer 0 Hardware
data HostSystem = HostSystem
    { hostId :: Text                  -- Unique Hostname or ID
    , hostHardwareRef :: SerialNumber -- Link to Layer0.Machine.machineSerial
    , hostOs :: OSType
    , hostKernelVersion :: Text
    , hostInterfaces :: [LogicalInterface]
    , hostProcesses :: [Process]
    , hostState :: SystemState
    } deriving (Show, Eq)

-- ==========================================
-- Infrastructure Roles (Context)
-- ==========================================

-- | Helper to define what this Host is doing in the datacenter
data HostRole
    = ComputeNode      -- Runs VMs (Layer 2)
    | StorageNode      -- Runs Storage (Ceph, etc.)
    | NetworkGateway   -- SDN Gateway / Firewall
    | ControllerNode   -- OpenStack/K8s Control Plane
    deriving (Show, Eq)
