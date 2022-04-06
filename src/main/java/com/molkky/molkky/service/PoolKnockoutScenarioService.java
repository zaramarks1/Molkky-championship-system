package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.repository.*;
import lombok.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

@Data
@Component
public class PoolKnockoutScenarioService {
    private Tournament tournament;

    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private SwissPoolRepository swissPoolRepository;
    @Autowired
    private KnockoutRepository knockoutRepository;
    @Autowired
    private MatchRepository matchRepository;

    public PoolKnockoutScenarioService() {
        this.tournament = new Tournament();
    }

    public void init(){
        tournamentRepository.save(this.tournament);
//        creer les rounds
        SwissPool swissPool = new SwissPool();

        Round swissRound = new Round();
        swissRound.setType("swissPool");
        swissRound.setNbTeams(4);
        swissRound.setTournament(this.tournament);
        roundRepository.save(swissRound);
        swissPool.setRound(swissRound);


        swissRound.setSwissPool(swissPool);
        swissPoolRepository.save(swissPool);

        Knockout knockout = new Knockout();

        Round knockOutRound = new Round();
        knockOutRound.setType("knockOut");
        knockOutRound.setNbTeams(2);
        knockOutRound.setTournament(this.tournament);
        roundRepository.save(knockOutRound);
        knockout.setRound(knockOutRound);

        knockOutRound.setKnockout(knockout);
        knockoutRepository.save(knockout);

        Set<Round> roundsTournament = new HashSet<>();
        roundsTournament.add(swissRound);
        roundsTournament.add(knockOutRound);
        this.tournament.setRounds(roundsTournament);

        this.tournament = tournamentRepository.save(this.tournament);
        System.out.println("Scénario initié");
    }

    public void start(){
        generateMatchesForPool();
    }

//    Permet de recuperer les matchs de la phase actuel,
//    un poolknockout est fait de pools pouis d'un knock out
//    donc il recupere celui des pools d'abord
    public List<Match> getCurrentPhaseMatches(){
        Integer indexPhase = this.tournament.getIndexPhase();
        List<Round> rounds = new ArrayList<>(this.tournament.getRounds());
        if(indexPhase == 0){
//            cas encore dans la pool
            return rounds.get(0).getPool().getMatches();
        } else{
//            cas dans le knock out
            return rounds.get(1).getPool().getMatches();
        }
    }

    public void setMatchScore(Match match, Integer scoreTeam1, Integer scoreTeam2){
        match.setScoreTeam1(scoreTeam1);
        match.setScoreTeam2(scoreTeam2);
        if(match.getScoreTeam1() == 50 || match.getScoreTeam2() == 50){
            match.setFinished(true);
            matchRepository.save(match);
            match.getSwissPool().setFinished(areAllMatchFinishedInPool(match.getSwissPool()));
            swissPoolRepository.save(match.getSwissPool());
            goToNextPhase();
        }
    }

    public void goToNextPhase(){
        if(this.tournament.getIndexPhase() == 0){
            this.tournament.setIndexPhase(this.tournament.getIndexPhase() + 1);
            this.tournament = tournamentRepository.save(this.tournament);
            generateMatchesForPool();
        } else {
            System.out.println("Fin du tournoi");
            tournament.setFinished(true);
            tournamentRepository.save(tournament);
        }
    }

    public Boolean areAllMatchFinishedInPool(SwissPool swissPool){
        Boolean finished = true;
        for (Match match : swissPool.getMatches()) {
            finished = match.getFinished();
        }
        return finished;
    }

//    generer les matchs pour la pool en cours
    public void generateMatchesForPool(){
        List<Round> rounds = new ArrayList<>(this.tournament.getRounds());
        List<Match> matches = new ArrayList<>();
        List<Team> teams = new ArrayList<>(this.tournament.getTeams());
//        pour la pool il faut un match contre tout le monde
        if(this.tournament.getIndexPhase() == 0){
            for(int i = 0; i < rounds.get(0).getNbTeams(); i++){
                for(int y = 0; y < rounds.get(0).getNbTeams(); y++){
                    if(i != y){
                        Match nvMatch = new Match();
                        Set<Team> teamsMatch = new HashSet<>();
                        teamsMatch.add(teams.get(i));
                        teamsMatch.add(teams.get(y));
                        nvMatch.setTeams(teamsMatch);
                        matches.add(nvMatch);
                    }
                }
            }
            rounds.get(0).getPool().setMatches(matches);
            roundRepository.save(rounds.get(0));
        }
        else if(this.tournament.getIndexPhase() == 1){
            ArrayList<Team> winningTeams = new ArrayList<>(getWinningTeamsFromPool());
            for(int i = 0; i < rounds.get(1).getNbTeams(); i++){
                for(int y = 0; y < rounds.get(1).getNbTeams(); y++){
                    if(i != y){
                        Match nvMatch = new Match();
                        Set<Team> teamsMatch = new HashSet<>();
                        teamsMatch.add(winningTeams.get(i));
                        teamsMatch.add(winningTeams.get(y));
                        nvMatch.setTeams(teamsMatch);
                        matches.add(nvMatch);
                    }
                }
            }
            rounds.get(1).getPool().setMatches(matches);
            roundRepository.save(rounds.get(1));
        }
    }

    public List<Team> getWinningTeamsFromPool(){
        List<Team> teams = new ArrayList<>(this.tournament.getTeams());
        teams.sort(Comparator.comparing(Team::getNbWins));
        ArrayList<Team> returnedTeams = new ArrayList<>();
        returnedTeams.add(teams.get(teams.size() - 1));
        returnedTeams.add(teams.get(teams.size() - 2));
        return returnedTeams;
    }

}
