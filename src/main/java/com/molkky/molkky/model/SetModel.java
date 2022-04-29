package com.molkky.molkky.model;

import com.molkky.molkky.domain.Set;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class SetModel {
    private Integer id;
    private Integer scoreTeam1 = 0;
    private Integer scoreTeam2 = 0;
    private Boolean finished = false;

    public SetModel(Set set) {
        this.id = set.getId();
        this.scoreTeam1 = set.getScoreTeam1();
        this.scoreTeam2 = set.getScoreTeam2();
        this.finished = set.getFinished();
    }

    public static List<SetModel> createSetModels(List<Set> setList) {
        List<SetModel> setModelList = new ArrayList<>();
        for (Set set : setList) {
            setModelList.add(new SetModel(set));
        }
        return setModelList;
    }
}
