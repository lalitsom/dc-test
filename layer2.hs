module VirtualLayer where

import Data.Text (Text)
import Data.Word (Word32)
import qualified OSLayer as L1
import qualified PhysicalLayer as L0

-- ==========================================
-- Layer 2: Virtualization & Tenancy
-- ==========================================

-- Concept:
-- This layer creates logical boundaries over the physical and host layers.
-- It introduces the concept of "Multi-tenancy" where resources are partitioned.

type TenantID = Text
type VmID = Text
type VNetID = Text
type SecGroupId = Text

-- ==========================================
-- 1. Ownership Boundary (Tenancy)
-- ==========================================

-- | The root boundary for resource ownership. 
-- Resources belonging to Tenant A are logically invisible to Tenant B.
data Tenant = Tenant
    { tenantId :: TenantID
    , tenantName :: Text
    , tenantBillingCode :: Text
    } deriving (Show, Eq)

-- ==========================================
-- 2. Network Boundary (Isolation)
-- ==========================================

-- | Defines how the network is isolated from others.
data IsolationMode
    = VLAN Word32      -- Layer 2 isolation tag
    | VXLAN Word32     -- Overlay network identifier (VNI)
    | GRE_Tunnel       -- Point-to-point encapsulation
    | Flat             -- No isolation (Public/Shared)
    deriving (Show, Eq)

-- | A Virtual Network (VPC) represents a private network space.
-- It acts as a container for Subnets and VMs.
data VirtualNetwork = VirtualNetwork
    { vnetId :: VNetID
    , vnetTenantId :: TenantID   -- Boundary Owner
    , vnetName :: Text
    , vnetCidr :: Text           -- e.g., "10.0.0.0/16"
    , vnetIsolation :: IsolationMode
    } deriving (Show, Eq)

-- ==========================================
-- 3. Security Boundary (Traffic Control)
-- ==========================================

data Protocol = TCP | UDP | ICMP | AnyProto deriving (Show, Eq)
data RuleDirection = Inbound | Outbound deriving (Show, Eq)
data RuleAction = Allow | Deny deriving (Show, Eq)

-- | Defines a hole in the boundary
data SecurityRule = SecurityRule
    { ruleDirection :: RuleDirection
    , ruleProtocol :: Protocol
    , rulePortRange :: (Word32, Word32) -- e.g., (80, 80) or (1024, 65535)
    , ruleRemoteCidr :: Text            -- Source IP for Inbound, Dest IP for Outbound
    , ruleAction :: RuleAction
    } deriving (Show, Eq)

-- | A Security Group acts as a distributed firewall around a group of VMs.
-- It is the primary "Protection Boundary".
data SecurityGroup = SecurityGroup
    { sgId :: SecGroupId
    , sgTenantId :: TenantID
    , sgName :: Text
    , sgRules :: [SecurityRule]
    } deriving (Show, Eq)

-- ==========================================
-- Compute Abstraction (Virtual Workloads)
-- ==========================================

data VmState
    = Provisioning
    | Running
    | Stopped
    | Paused
    | ErrorState
    deriving (Show, Eq)

-- | A Virtual Machine is a compute unit running inside the boundaries.
data VirtualMachine = VirtualMachine
    { vmId :: VmID
    , vmTenantId :: TenantID         -- Ownership Boundary
    , vmName :: Text
    , vmImageRef :: Text             -- OS Image
    , vmVCpuCount :: Word32
    , vmRamMB :: Word32
    
    -- Placement: Where is it physically running?
    -- This bridges Layer 2 back to Layer 1 (HostSystem)
    , vmHostBinding :: Maybe L1.HostSystem 
    
    -- Networking: Connected to which Network Boundary?
    , vmNetworkRef :: VNetID
    , vmPrivateIp :: Text
    
    -- Security: Protected by which Security Boundaries?
    , vmSecurityGroups :: [SecGroupId]
    
    , vmState :: VmState
    } deriving (Show, Eq)

-- ==========================================
-- The Logical Cloud View
-- ==========================================

-- | The aggregate view of the virtualized infrastructure
data VirtualDatacenter = VirtualDatacenter
    { vdcRegion :: Text
    , vdcTenants :: [Tenant]
    , vdcNetworks :: [VirtualNetwork]
    , vdcSecurityGroups :: [SecurityGroup]
    , vdcWorkloads :: [VirtualMachine]
    } deriving (Show, Eq)
