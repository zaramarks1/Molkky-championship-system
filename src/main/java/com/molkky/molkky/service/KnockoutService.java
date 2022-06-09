package com.molkky.molkky.service;


import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.TeamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class KnockoutService {

    @Autowired
    RoundRepository roundRepository;

    @Autowired
    MatchRepository matchRepository;

    @Autowired
    TeamRepository teamRepository;

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    RoundService roundService;

    @Autowired
    NotificationService notificationService;


    Map<Round, List<Match>> generateRounds(Knockout knockout) {
        return roundService.generateRoundKnockoutAndSwiss(knockout);
    }

    void validateRound(Round round){

        List<PhaseRankingModel>  scoresList =  roundService.orderTeamsByScoreInRound(round, 2);

        int nbEliminated = scoresList.size()/2;

        for(int i = nbEliminated; i < scoresList.size();i++){
            scoresList.get(i).getTeam().setEliminated(true);
        }

        List<Team> teams =roundService.seedingSystem(round, scoresList);

        generateNotificationAfterRound(teams);

        roundService.isPhaseOver(round.getPhase(), scoresList);

    }

    void generateNotificationAfterRound(List<Team> teams){

        for (Team t : teams) {
            String message;
            if (t.isEliminated()) {
                message = "Ton équipe a malheureuseusement été disqualifiée dans la phase de tableau éliminatoire";
            } else {
                message = " Felicitations! Ton équipe est qualifiée pour la prochaine phase de tableau éliminatoire";
            }

            notificationService.sendNotificationToList(message, "", t.getUserTournamentRoles());

        }
    }
}
