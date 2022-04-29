package com.molkky.molkky.model;

import com.molkky.molkky.domain.Round;
import lombok.Data;
import type.RoundType;

import java.util.ArrayList;
import java.util.List;

@Data
public class RoundModel {
    private Integer id;
    private RoundType type;
    private Integer nbTeams;
    private Integer nbSets;
    private List<TeamModel> teams;
    private List<MatchModel> matches = new ArrayList<>();
    private Boolean finished = false;
    private TournamentModel tournament;

    public RoundModel(Round round) {
        this.id = round.getId();
        this.type = round.getType();
        this.nbTeams = round.getNbTeams();
        this.nbSets = round.getNbSets();
        this.teams = TeamModel.createTeamModels(round.getTeams());
        this.matches = MatchModel.createMatchModels(round.getMatches());
        this.tournament = new TournamentModel(round.getTournament());
    }

}
