package com.molkky.molkky.model;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class SetModel {
    private Integer id;
    private Integer score1Team1 = 0;
    private Integer score2Team1 = 0;
    private Integer score1Team2 = 0;
    private Integer score2Team2 = 0;
    private Integer score1Orga = 0;
    private Integer score2Orga = 0;
    private Boolean finished = false;
}
