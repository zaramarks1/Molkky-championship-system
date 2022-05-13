package com.molkky.molkky.model;

import com.molkky.molkky.domain.Round;
import lombok.Data;
import lombok.NoArgsConstructor;
import type.PhaseType;

@Data
@NoArgsConstructor
public class RoundModel {
    private Integer id;
    private PhaseType type;
    private Integer nbTeams;
    private Boolean finished = false;

    public RoundModel(Round round) {
        this.id = round.getId();
        this.type = round.getType();
        this.nbTeams = round.getNbTeams();
        this.finished = round.getFinished();
    }

}
