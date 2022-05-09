package com.molkky.molkky.domain.rounds;

import com.molkky.molkky.domain.Phase;
import type.PhaseType;
import com.molkky.molkky.domain.Round;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Getter
@Entity
@Setter
@DiscriminatorValue("POOL")
public class Pool extends Phase {


    @Column(name = "victoryValue")
    private Integer victoryValue;

    @Column(name = "notifBeginningPhase")
    private boolean notifBeginningPhase;

    @Column(name = "notifEachDay")
    private boolean notifEachDay;

    @Column(name = "nbTeamsQualified")
    private Integer nbTeamsQualified;

    @Column(name = "nbPools")
    private Integer nbPools;

    public Pool(){
        this.notifBeginningPhase = false;
        this.notifEachDay = false;
    }

}
