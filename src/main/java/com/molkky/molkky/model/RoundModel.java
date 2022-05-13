package com.molkky.molkky.model;

import com.molkky.molkky.domain.Round;
import lombok.Data;
import lombok.NoArgsConstructor;
import type.RoundType;

@Data
@NoArgsConstructor
public class RoundModel {
    private Integer id;
    private RoundType type;
    private Integer nbTeams;
    private Integer nbSets;
    private Boolean finished = false;

    public RoundModel(Round round) {
        this.id = round.getId();
        this.type = round.getType();
        this.nbTeams = round.getNbTeams();
        this.nbSets = round.getNbSets();
        this.finished = round.getFinished();
    }

}
