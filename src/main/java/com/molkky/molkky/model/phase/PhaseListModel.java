package com.molkky.molkky.model.phase;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class PhaseListModel {
    List<PhaseModel> phases = new ArrayList<>();

    public void add(PhaseModel phase){
        this.phases.add(phase);
    }
}
