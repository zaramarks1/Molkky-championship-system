package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class RoundService {

    public List<PhaseRankingModel> orderTeamsByScoreInRound(Round round, int victoryValue){
        Map<Team, PhaseRankingModel> scores = new HashMap<>();
        List<PhaseRankingModel> scoresList = new ArrayList<>();

        for(Team t : round.getTeams()){
            PhaseRankingModel phaseRankingModel = new PhaseRankingModel();
            phaseRankingModel.setTeam(t);
            scores.put(t, phaseRankingModel);
        }

        for(Match m : round.getMatches()){
            Team team1 = m.getTeams().get(0);
            Team team2 = m.getTeams().get(1);

            PhaseRankingModel phaseRankingModel1 = scores.get(team1);
            phaseRankingModel1.setTeam(team1);
            phaseRankingModel1.setTotalPoints(phaseRankingModel1.getTotalPoints() + m.getScoreTeam1());
            PhaseRankingModel phaseRankingModel2 = scores.get(team2);
            phaseRankingModel2.setTeam(team2);
            phaseRankingModel2.setTotalPoints(phaseRankingModel2.getTotalPoints() + m.getScoreTeam2());

            for(Team t : m.getTeams()){
                if(t.getId().equals(team1.getId()) && t.getId().equals(m.getWinner().getId())){
                    phaseRankingModel1.setValues(phaseRankingModel1.getValues() + victoryValue);
                }else  if(t.getId().equals(team2.getId()) && t.getId().equals(m.getWinner().getId())){
                    phaseRankingModel2.setValues(phaseRankingModel2.getValues() + victoryValue);
                }
            }


            scores.put(team1, phaseRankingModel1);
            scores.put(team2, phaseRankingModel2);
        }

        for(Map.Entry<Team, PhaseRankingModel> entry : scores.entrySet()){

            Team team = entry.getKey();
            PhaseRankingModel phaseRankingModel = scores.get(team);

            scoresList.add(phaseRankingModel);


        }

        scoresList.sort(Comparator
                .comparing(PhaseRankingModel::getValues)
                .thenComparing(PhaseRankingModel::getTotalPoints)
                .reversed());

        return scoresList;

    }
}
