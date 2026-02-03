Host are of these types : 
1. Management_Switch
2. Node (Layer 2 will have the segregation of Control and Data Plane nodes)
3. ToR_L3_Switch
4. Spine_Switch
5. Firewall
6. MaaS_Host

**Example DSL**
Layer Layer0:
    Datacenter nxtGen:
        Layer: 0
        Description:
        .
        .
        .
        Rack R1:
            Size: 42u
            Machine M1:
                Name: "HPExxx"
                Size: 
                Nics:
                    Nic1:
                        Type: RJ45
                        PortNumber: 1
                        MacAddress : 
                        Bandwidth:
                    Nic2:
                        type: . . . .
                Cores: . . . . 
                Memory: . . . .
                Storage: . . . .
                Size: 4U
            Machine M2:
                Name: "Arista Switch"
                Type: ToR_L3_Switch ## 
                Size: 
                Nics:
                    ......
        Cables:
            Cable1:
                type: RJ45
                EndpointA: R1.M1.Nic1
                EndpointB: R1 . . . . 

Constraints at Layer 0 - 
- No 2 endpoints should be same in Cables definition.
- Bandwidth 
- Every Rack should have 2 TOR_L3_Switch
- Every Rack TOR_L3_Switch should be also connected to minimum 2 Spine_L3_Switch
- It shouldn't be possible for sum of sizes of Machines within a Rack goes beyond the rack size.
- Network uplink capacity ≥ Σ(hypervisor_capacity)


Layer Layer1:
    Host1:
        Machine : Layer0.nxtGen.R1.M1
        Type: Node
        OS:
            Name: Ubuntu | Redhat
            Version: Ubuntu-24.04
            PhysicalInterfaces:
                en01:
                    NIC: Layer0.nxtGen.R1.M1.Nic1
                    ip: 192.168.0.54/24
                    Type: Management
                en02: ....
        

    Host2:
        Machine : Layer0.nxtGen.R1.M2
        Type: Spine Switch
        OS:
            Name: "Arsita Switch OS"
        . . . .
    

Constraints at Layer 1 - 
- Every Host should have exactly one Interface with type Management and atleast one more.
- There must be atleast 1 Host with Type ToR_L3_Switch.
- 



Layer Layer2:
    M1H1:
        Type: DataPlaneNode | ControlPlaneNode | MaaS
        Processes:
            MaaS:
            SSH:
            GIT:
                Configuration: CouldBePathToFileInSameRepo
                Version:
            Nova:
            OVS:
            Ceph:
            Python3
            # All the processes for any machine would be mapped here.
        Interfaces:
            verth:
                .....
            br-ex:
                interface:
                    Layer2.M1H1.Interfaces.en01
                    Layer2.M1H1.Interfaces.veth
                    

Constraints at Layer 2 - 

- Every Node should have atleast one Interface of type br-ex
- If a Node is of type MaaS then it should have process MaaS running.
- If a Node is of type ControlPlaneNode then it should have process OVS running.
- If a Node is of type DataPlaneNode then it should have process Nova running.
- All node should have same version of all processes running.