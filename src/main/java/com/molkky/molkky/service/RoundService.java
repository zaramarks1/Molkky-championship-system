package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.TeamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class RoundService {

    @Autowired
    TeamRepository teamRepository;

    public List<PhaseRankingModel> orderTeamsByScoreInRound(Round round, int victoryValue){
        Map<Integer, PhaseRankingModel> scores = new HashMap<>();
        List<PhaseRankingModel> scoresList = new ArrayList<>();

        for(Team t : round.getTeams()){
            PhaseRankingModel phaseRankingModel = new PhaseRankingModel();
            phaseRankingModel.setTeam(t);
            scores.put(t.getId(), phaseRankingModel);
        }

        for(Match m : round.getMatches()){
            Team team1 = m.getTeams().get(0);
            Team team2 = m.getTeams().get(1);

            PhaseRankingModel phaseRankingModel1 = scores.get(team1.getId());
            phaseRankingModel1.setTeam(team1);
            phaseRankingModel1.setTotalPoints(phaseRankingModel1.getTotalPoints() + m.getScoreTeam1());
            PhaseRankingModel phaseRankingModel2 = scores.get(team2.getId());
            phaseRankingModel2.setTeam(team2);
            phaseRankingModel2.setTotalPoints(phaseRankingModel2.getTotalPoints() + m.getScoreTeam2());

            for(Team t : m.getTeams()){
                if(m.getWinner()!= null){
                    if(t.getId().equals(team1.getId()) && t.getId().equals(m.getWinner().getId())){
                        phaseRankingModel1.setValues(phaseRankingModel1.getValues() + victoryValue);
                    }else  if(t.getId().equals(team2.getId()) && t.getId().equals(m.getWinner().getId())){
                        phaseRankingModel2.setValues(phaseRankingModel2.getValues() + victoryValue);
                    }
                }
            }


            scores.put(team1.getId(), phaseRankingModel1);
            scores.put(team2.getId(), phaseRankingModel2);
        }

        for(Map.Entry<Integer, PhaseRankingModel> entry : scores.entrySet()){

            Team team = teamRepository.findById(entry.getKey());
            PhaseRankingModel phaseRankingModel = scores.get(team.getId());

            scoresList.add(phaseRankingModel);


        }

        scoresList.sort(Comparator
                .comparing(PhaseRankingModel::getValues)
                .thenComparing(PhaseRankingModel::getTotalPoints)
                .reversed());

        return scoresList;

    }

    public List<Match> createSetsFromMatch(List<Match> matches){
        int nbSets = matches.get(0).getRound().getPhase().getNbSets();
        List<Match> results  = new ArrayList<>();

        for(Match m : matches){
            List<com.molkky.molkky.domain.Set> sets = new ArrayList<>();
            for(int i =0; i <nbSets; i++){
                Set set = new Set();
                set.setTeams(m.getTeams());
                set.setMatch(m);
                sets.add(set);
            }
            m.setSets(sets);
            results.add(m);
        }
        return results;
    }
}
