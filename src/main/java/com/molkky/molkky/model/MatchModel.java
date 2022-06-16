package com.molkky.molkky.model;

import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class MatchModel {
    private Integer id;
    private Boolean finished;
    private Integer nbSets;
    private Team winner;
    private Round round;
}
