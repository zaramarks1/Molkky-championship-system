package com.molkky.molkky.model;

import com.molkky.molkky.domain.Tournament;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.AssertTrue;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;

@Data
@NoArgsConstructor
public class TournamentModel {
    private String name;
    private String location;
    private Date date;
    private Date cutOffDate;
    private Integer minTeam;
    private Integer maxTeam;
    private Integer nbRounds;
    private Integer nbCourts;
    private Integer nbPlayersPerTeam;
    private boolean visible;
    private String email;

    public void setDate(String date) throws ParseException {
        if(!Objects.equals(date, "")) this.date = formatDate(date);
    }

    public void setCutOffDate(String cutOffDate) throws ParseException {
        if(!Objects.equals(cutOffDate, "")) this.cutOffDate = formatDate(cutOffDate);
    }

    public Date formatDate(String date) throws ParseException {
        return new SimpleDateFormat("yyyy-MM-dd", Locale.FRANCE).parse(date);
    }

    @AssertTrue(message = "Date du tournoi avant la date de fin d'inscriptions")
    // Other rules can also be validated in other methods
    public boolean isCutoffDateBeforeDate() {
        return cutOffDate.before(date) || cutOffDate.equals(date);
    }

    public TournamentModel(Tournament tournament) {
        if(tournament.getId() != null) {
            this.name = tournament.getName();
            this.location = tournament.getLocation();
            this.date = tournament.getDate();
            this.cutOffDate = tournament.getCutOffDate();
            this.minTeam = tournament.getMinTeam();
            this.maxTeam = tournament.getMaxTeam();
            this.nbRounds = tournament.getNbRounds();
            this.nbCourts = tournament.getNbCourts();
            this.nbPlayersPerTeam = tournament.getNbPlayersPerTeam();
            this.visible = tournament.isVisible();
        }
    }

}
