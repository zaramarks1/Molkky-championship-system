package com.molkky.molkky.model.phase;

import com.molkky.molkky.domain.Team;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class PhaseRankingModel {

    Team team;
    Integer totalPoints = 0;
    Integer values = 0;


}
