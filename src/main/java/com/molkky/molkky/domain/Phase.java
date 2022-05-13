package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import type.PhaseStatus;

import javax.persistence.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Entity
@NoArgsConstructor
@Table(name = "phase")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name="typeDiscriminator", discriminatorType = DiscriminatorType.STRING)
@DiscriminatorValue("PHASE")
public class Phase  implements Serializable {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "nbPhase")
    private Integer nbPhase;

    @Column(name = "nbSets")
    private Integer nbSets;

    @Column(name = "status")
    private PhaseStatus status;

    @Column(name = "isRanking")
    private Boolean ranking = false;

    @Column(name = "isSeedingSystem")
    private Boolean seedingSystem = false;

    @Column(name = "isTerrainAffectation")
    private Boolean terrainAffectation = false;

    @ManyToOne
    @JoinColumn(name="tournament_id", nullable=false)
    private Tournament tournament;

    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "phase_id")
    private List<Round> rounds = new ArrayList<>();



}
